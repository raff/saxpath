package main

import (
	"encoding/xml"
	"errors"
	"flag"
	"io"
	"log"
	"os"
	"regexp"
	"strings"
)

var (
	Debug = false

	ParseComplete = errors.New("parsing complete")
	ParseStopped  = errors.New("parsing stopped")
	ReadComplete  = io.EOF
	InvalidToken  = errors.New("invalid token")

	AnyName = xml.Name{Local: "*"}
	AnyAttr = xml.Attr{Name: AnyName, Value: "*"}
	NoAttr  = xml.Attr{}

	// ugly regexp matching the basic XPath predicate expression [ @name = 'value' ]
	pAttribute = regexp.MustCompile(`^\\[\\s*@(.*)\\s*(=)\\s*["']?(.*?)["']?\\s*\\]`)
)

func dlog(args ...interface{}) {
	if Debug {
		log.Println(args...)
	}
}

func dlogf(fmt string, args ...interface{}) {
	if Debug {
		log.Printf(fmt, args...)
	}
}

func ns(e xml.Name) string {
	if e.Space != "" {
		return e.Space + ":" + e.Local
	} else {
		return e.Local
	}
}

////////////////////////////////////////

// SAX-like handler
type SAXHandler interface {
	//called when XML document start
	StartDocument() bool
	//called when XML document end
	EndDocument() bool
	//called when XML tag start
	StartElement(xml.StartElement) bool
	//called when XML tag end
	EndElement(xml.EndElement) bool
	//called when the parser encount chardata
	CharData(xml.CharData) bool
	//called when the parser encount comment
	Comment(xml.Comment) bool
	//called when the parser encount procInst
	//<!procinst >
	ProcInst(xml.ProcInst) bool
	//called when the parser encount directive
	//
	Directive(xml.Directive) bool
}

////////////////////////////////////////

// SAX-like XML Parser
type SAXParser struct {
	*xml.Decoder
	handler SAXHandler
	started bool
	ended   bool
}

// Create a New SAXParser
func NewSAXParser(reader io.Reader, handler SAXHandler) *SAXParser {
	decoder := xml.NewDecoder(reader)
	return &SAXParser{Decoder: decoder, handler: handler}
}

// SetHTMLMode make Parser can parse invalid HTML
func (p *SAXParser) SetHTMLMode() {
	p.Strict = false
	p.AutoClose = xml.HTMLAutoClose
	p.Entity = xml.HTMLEntity
}

// Parse calls handler's methods
// when the parser encount a start-element,a end-element, a comment and so on.
//
// The parsing process stops if the handler methods return "true" and can be restarted
// by calling Parse again until it returns ParseComplete or ReadComplete
func (p *SAXParser) Parse() (xml.Token, error) {
	if p.ended {
		return nil, ParseComplete
	}

	if !p.started {
		p.started = true
		p.handler.StartDocument()
	}

	stop := false

	for {
		token, err := p.Token()
		if err == io.EOF {
			return nil, ReadComplete
		}
		if err != nil {
			return nil, err
		}

		switch t := token.(type) {
		case xml.StartElement:
			stop = p.handler.StartElement(t)
		case xml.EndElement:
			stop = p.handler.EndElement(t)
		case xml.CharData:
			stop = p.handler.CharData(t)
		case xml.Comment:
			stop = p.handler.Comment(t)
		case xml.ProcInst:
			stop = p.handler.ProcInst(t)
		case xml.Directive:
			stop = p.handler.Directive(t)
		default:
			panic("unknown xml token")
		}

		if stop {
			return token, ParseStopped
		}

	}

	p.ended = true
	p.handler.EndDocument()
	return nil, ParseComplete
}

////////////////////////////////////////

type Matches int

const (
	MATCH_NO Matches = iota
	MATCH_PART
	MATCH_PATTERN
	MATCH_DESCENDANT
	MATCH_PARTIAL
	MATCH_TEXT
	MATCH_NOTEXT
)

////////////////////////////////////////

// XPattern is a structure to store pattern (paths) matches
type XPattern struct {
	parts []string
	attrs []xml.Attr
	texts []string
	start int
	curr  int
}

// NewXPattern builds the "parts" list (elements) and the attrs list (attributes)
func NewXPattern(pattern string) *XPattern {
	x := &XPattern{}

	x.parts = strings.Split(pattern, "/")
	x.attrs = make([]xml.Attr, len(x.parts))
	x.texts = make([]string, len(x.parts))

	for i, part := range x.parts {
		if p := strings.Index(part, "["); p >= 0 {
			matches := pAttribute.FindStringSubmatch(part[p:])
			x.parts[i] = part[:p]

			if matches != nil {
				x.attrs[i] = xml.Attr{Name: xml.Name{Local: matches[1]}, Value: matches[3]}
			}
			continue
		}

		if p := strings.Index(part, "="); p >= 0 {
			value := part[p:]
			x.parts[i] = part[:p]

			if strings.HasPrefix(value, `'`) {
				value = strings.Trim(value, `'`)
			} else if strings.HasPrefix(value, `"`) {
				value = strings.Trim(value, `"`)
			}

			x.texts[i] = value
		}
	}

	x.start = -1
	x.curr = -1
	return x
}

// CloneXPattern creates a copy if the input pattern
func CloneXPattern(p *XPattern) *XPattern {
	x := &XPattern{
		parts: make([]string, len(p.parts)),
		attrs: make([]xml.Attr, len(p.attrs)),
		texts: make([]string, len(p.texts)),
		start: -1,
		curr:  -1,
	}

	copy(x.parts, p.parts)
	copy(x.attrs, p.attrs)
	copy(x.texts, p.texts)
	return x
}

// matchName matches an element name. '*' means any name
func (p *XPattern) matchName(name xml.Name, i int) bool {
	return p.parts[i] == "*" || p.parts[i] == name.Local
}

// matchAttr matches an element attribute. nil means any attribute
func (p *XPattern) matchAttr(attr []xml.Attr, i int) bool {
	mattr := p.attrs[i]

	if mattr == NoAttr {
		return true
	}

	for _, a := range attr {
		if mattr == a {
			return true
		}
	}

	return false
}

// matchRoot matches the root of the path
func (p *XPattern) matchRoot(name xml.Name, attr []xml.Attr) bool {
	return p.matchName(name, 0) && p.matchAttr(attr, 0)
}

// matchLevel matches start level and name (use to match at end of elements)
func (p *XPattern) matchLevel(name xml.Name, level int) bool {
	return level == p.start && p.matchName(name, 0)
}

// matchPAth matches the Matches full path (use to match at start of element)
func (p *XPattern) matchPath(name xml.Name, attr []xml.Attr, level int) Matches {
	pos := 0

	if p.start >= 0 {
		if level < p.start {
			p.start = -1
			p.curr = -1
			return MATCH_NO
		}

		switch {
		case level-p.start >= len(p.parts):
			if p.curr-p.start == len(p.parts) {
				return MATCH_DESCENDANT
			} else {
				return MATCH_PARTIAL
			}

		case level > p.curr:
			return MATCH_PARTIAL

		case level < p.curr:
			p.curr = level
		}

		pos = p.curr - p.start
	}

	if !p.matchName(name, pos) {
		if p.start >= 0 {
			return MATCH_PARTIAL
		} else {
			return MATCH_NO
		}
	}

	if !p.matchAttr(attr, pos) {
		if p.start >= 0 {
			return MATCH_PARTIAL
		} else {
			return MATCH_NO
		}
	}

	if p.start < 0 {
		p.start = level
	}

	p.curr = level + 1

	if pos == len(p.parts)-1 {
		if p.texts[pos] == "" {
			return MATCH_PATTERN
		} else {
			return MATCH_TEXT
		}
	} else {
		return MATCH_PART
	}
}

// matchText matches the text for an element (in EndElement)
func (p *XPattern) matchText(chars xml.CharData) Matches {
	text := string(chars)
	last := len(p.parts) - 1

	switch {
	case p.texts[last] == "":
		return MATCH_NOTEXT

	case p.texts[last] == text:
		return MATCH_PATTERN

	default:
		return MATCH_NO
	}
}

////////////////////////////////////////

type SAXFinder struct {
	pattern   *XPattern    // pattern to find
	matching  []*XPattern  // pattern that matches
	current   string       // current path
	text      xml.CharData // current text
	attrs     []xml.Attr   // current attributes
	level     int          // current level
	lastMatch Matches
}

func NewSAXFinder() *SAXFinder {
	return &SAXFinder{
		level:     -1,
		lastMatch: MATCH_NO,
	}
}

func (h *SAXFinder) SetPattern(pattern string) {
	h.pattern = NewXPattern(pattern)
	h.matching = nil
	h.current = ""
	h.text = nil
	h.attrs = nil
	h.level = -1
	h.lastMatch = MATCH_NO
}

func (h *SAXFinder) StartDocument() (stop bool) {
	dlog("doc start")

	h.current = ""
	h.text = nil
	h.attrs = nil
	h.level = -1
	h.lastMatch = MATCH_NO
	return
}

func (h *SAXFinder) EndDocument() (stop bool) {
	dlog("doc end")
	return
}

func (h *SAXFinder) StartElement(element xml.StartElement) (stop bool) {
	dlog("start", ns(element.Name))

	h.text = nil
	h.attrs = element.Attr
	h.level += 1
	h.current += "/" + element.Name.Local

	if h.pattern.matchRoot(element.Name, element.Attr) {
		h.matching = append(h.matching, CloneXPattern(h.pattern))
	}

	var match *XPattern

	//
	// Process current matching patterns
	//
	for _, p := range h.matching {
		h.lastMatch = p.matchPath(element.Name, element.Attr, h.level)

		// If full match, we are done
		if h.lastMatch == MATCH_PATTERN {
			match = p
			break
		}

		if h.lastMatch == MATCH_TEXT {
			break
		}
	}

	if match != nil {
		return true
	}

	return
}

func (h *SAXFinder) EndElement(element xml.EndElement) (stop bool) {
	dlog("end", ns(element.Name))

	/*
	 * See if we have any pending element to start
	 */
	if h.lastMatch == MATCH_TEXT {
		for i := 0; i < len(h.matching); i++ {
			p := h.matching[i]

			match := p.matchText(h.text)
			if match == MATCH_NOTEXT { // do the element
				h.lastMatch = match
				break
			}

			if match == MATCH_PATTERN {
				h.lastMatch = match
				break
			}
		}

		if h.lastMatch == MATCH_TEXT { // we didn't match
			h.lastMatch = MATCH_NO
		}
	}

	for i := len(h.matching) - 1; i >= 0; i-- {
		p := h.matching[i]

		if p.matchLevel(element.Name, h.level) {
			h.matching = append(h.matching[:i], h.matching[i+1:]...)
		}
	}

	l := strings.LastIndex(h.current, "/")
	if l >= 0 {
		h.current = h.current[:l]
		h.level--
	}

	h.text = nil
	h.attrs = nil

	return h.lastMatch == MATCH_PATTERN
}

func (h *SAXFinder) Comment(comment xml.Comment) (stop bool) {
	dlog("comment", string(comment))
	return
}

func (h *SAXFinder) CharData(chars xml.CharData) (stop bool) {
	dlog("chardata", string(chars))

	h.text = append(h.text, chars...)
	return
}

func (h *SAXFinder) ProcInst(proc xml.ProcInst) (stop bool) {
	dlog("proc", proc.Target, string(proc.Inst))
	return
}

func (h *SAXFinder) Directive(dir xml.Directive) (stop bool) {
	dlog("directive", string(dir))
	return
}

////////////////////////////////////////

func NewFinder(r io.Reader) *SAXParser {
	return NewSAXParser(r, NewSAXFinder())
}

func (p *SAXParser) FindElement(pattern string, res interface{}) error {
	p.handler.(*SAXFinder).SetPattern(pattern)
	t, err := p.Parse()

	dlogf("Token: %#v, State: %v", t, err)

	if err != ParseStopped || res == nil {
		return err
	}

	if s, ok := t.(xml.StartElement); ok {
		dlog("decode", s)
		return p.DecodeElement(res, &s)
	}

	return InvalidToken
}

////////////////////////////////////////

func main() {
	flag.BoolVar(&Debug, "debug", false, "print debug messages")
	flag.Parse()

	var filename, xpattern string

	switch flag.NArg() {
	case 1:
		filename = "-"
		xpattern = flag.Arg(0)

	case 2:
		filename = flag.Arg(0)
		xpattern = flag.Arg(1)

	default:
		log.Fatal("usage: saxpath [--debug] [input.xml] {xpath}")
	}

	var r io.Reader

	if filename == "-" {
		r = os.Stdin
	} else {
		f, err := os.Open(filename)
		if err != nil {
			log.Fatal(err)
		}
		defer f.Close()
		r = f
	}

	//var res struct {
	//    Value string `xml:",chardata"`
	//}

	var res string
	var err error

	finder := NewFinder(r)

	for err == nil {
		log.Println("-----------------------------------------")
		err = finder.FindElement(xpattern, &res)
		log.Println(res)
	}

	log.Println(err)
}
