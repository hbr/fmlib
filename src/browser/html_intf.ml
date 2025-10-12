module type S =
sig
    type _ attr

    (** Virtual Dom *)


    type 'msg t (** Type of a virtual dom node potentially generating a message
                    of type ['msg]. *)

    val text: string -> 'msg t (** [text str] Create a text node. *)

    val node: string -> 'msg attr list -> 'msg t list -> 'msg t
    (** [node tag attrs children]

        Create an html element with a tagname, a list of attributes and a list
        of children.
    *)

    val node_ns:
        string -> string -> 'msg attr list -> 'msg t list -> 'msg t
    (** [node namespace tag attrs children]

        Like [node], but creates the node within a namespace e.g.
        "http://www.w3.org/2000/svg" for [svg] elements.
    *)



    val svg_node: string -> 'msg attr list -> 'msg t list -> 'msg t
    (** [svg_node tag attrs children]

        Create an svg element with a tagname, a list of attributes and a list
        of children. An svg element is a node in the namespace
        "http://www.w3.org/2000/svg".
    *)



    val map: ('a -> 'b) -> 'a t -> 'b t
    (** [map f vdom]

        Map a virtual dom [vdom] creating messages of type ['a] to a virtual dom
        creating messages of type ['b].
    *)




    val keyed:
        string -> 'msg attr list -> (string * 'msg t) list -> 'msg t
    (** [keyed tag attrs children]

        Like [node], but add a unique identifier to each child node. This makes
        adding, removing and modifying child nodes more efficient. The dom
        diffing algorithm compares child nodes with the same identifier.
     *)




    (** {1 Content sectioning} *)

    val address: 'msg attr list -> 'msg t list -> 'msg t
    (** [address attrs children]

        Defines a section containing contact information about a person or
        organization.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/address
    *)

    val article: 'msg attr list -> 'msg t list -> 'msg t
    (** [article attrs children]

        Defines self-contained content, which is usable independently of
        the page. Examples include blog posts, user-submitted comments or
        interactive widgets.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/article
    *)

    val aside: 'msg attr list -> 'msg t list -> 'msg t
    (** [aside attrs children]

        Defines content that is not directly related to the main content of the page.
        This content is often presented as a sidebar or a call-out box.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/aside
    *)

    val footer: 'msg attr list -> 'msg t list -> 'msg t
    (** [footer attrs children]

        Defines the footer for a page or section. It typically contains
        information about the author, copyright data, or links to related
        documents.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/footer
    *)

    val header: 'msg attr list -> 'msg t list -> 'msg t
    (** [header attrs children]

        Defines the header of a page or section. It typically contains a logo,
        the title of the page and navigation elements.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/header
    *)

    val h1: 'msg attr list -> 'msg t list -> 'msg t
    (** [h1 attrs children]

        Defines a section heading. <h1> is the highest section level and <h6> is
        the lowest.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/Heading_Elements
    *)

    val h2: 'msg attr list -> 'msg t list -> 'msg t
    (** [h2 attrs children]

        Defines a section heading. [h1] is the highest section level and [h6] is
        the lowest.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/Heading_Elements
    *)

    val h3: 'msg attr list -> 'msg t list -> 'msg t
    (** [h3 attrs children]

        Defines a section heading. [h1] is the highest section level and [h6] is
        the lowest.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/Heading_Elements
    *)

    val h4: 'msg attr list -> 'msg t list -> 'msg t
    (** [h4 attrs children]

        Defines a section heading. [h1] is the highest section level and [h6] is
        the lowest.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/Heading_Elements
    *)

    val h5: 'msg attr list -> 'msg t list -> 'msg t
    (** [h5 attrs children]

        Defines a section heading. [h1] is the highest section level and [h6] is
        the lowest.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/Heading_Elements
    *)

    val h6: 'msg attr list -> 'msg t list -> 'msg t
    (** [h6 attrs children]

        Defines a section heading. [h1] is the highest section level and [h6] is
        the lowest.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/Heading_Elements
    *)

    val hgroup: 'msg attr list -> 'msg t list -> 'msg t
    (** [hgroup attrs children]

        Represents a heading grouped with any secondary content, such as
        subheadings, an alternative title, or a tagline.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/hgroup
    *)

    val main: 'msg attr list -> 'msg t list -> 'msg t
    (** [main attrs children]

        Defines the main or most important content of the page. There can be
        only one main element.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/main
    *)

    val nav: 'msg attr list -> 'msg t list -> 'msg t
    (** [nav attrs children]

        Represents a section that provides navigation links.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/nav
    *)

    val section: 'msg attr list -> 'msg t list -> 'msg t
    (** [section attrs children]

        Represents a generic section of a document. Use this if the content
        cannot be represented by a more specific semantic element. A section
        should have a heading.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/section
    *)

    val search: 'msg attr list -> 'msg t list -> 'msg t
    (** [search attrs children]

        Represents a section that contains controls for performing a search or
        filtering operation.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/search
    *)




    (** {1 Text content} *)

    val blockquote: 'msg attr list -> 'msg t list -> 'msg t
    (** [blockquote attrs children]

        Represents a block of quoted text. The [cite] attribute allows
        specifying a source URL and the title of the source can be given using
        the {!cite} element.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/blockquote
    *)

    val dd: 'msg attr list -> 'msg t list -> 'msg t
    (** [dd attrs children]

        Provides the definition or description of the {!dt} element listed
        before it.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/dd
    *)

    val div: 'msg attr list -> 'msg t list -> 'msg t
    (** [div attrs children]

        A generic container. More specific elements such as {!article} or
        {!main} should be used instead if their semantics apply to the content.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/div
    *)

    val dl: 'msg attr list -> 'msg t list -> 'msg t
    (** [dl attrs children]

        Defines a definition list containing terms ({!dt} elements) and their
        definitions ({!dd} elements).

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/dl
    *)

    val dt: 'msg attr list -> 'msg t list -> 'msg t
    (** [dt attrs children]

        Represents a term which is defined / described by the subsequent {!dd}
        element.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/dt
    *)

    val figcaption: 'msg attr list -> 'msg t list -> 'msg t
    (** [figcaption attrs children]

        Represents the caption or legend of a figure.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/figcaption
    *)

    val figure: 'msg attr list -> 'msg t list -> 'msg t
    (** [figure attrs children]

        Defines a figure which contains some content and a {!figcaption}.
        Examples for suitable content are pictures, diagrams, quotes or code
        snippets.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/figure
    *)

    val hr: 'msg attr list -> 'msg t list -> 'msg t
    (** [hr attrs children]

        Represents a thematic break between paragraphs.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/hr
    *)

    val li: 'msg attr list -> 'msg t list -> 'msg t
    (** [li attrs children]

        Defines an item of an ordered or unordered list.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/li
    *)

    val menu: 'msg attr list -> 'msg t list -> 'msg t
    (** [menu attrs children]

        Represents a list of menu entries, such as navigation links or buttons.
        This can be used as a semantic alternative to {!ol}.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/menu
    *)

    val ol: 'msg attr list -> 'msg t list -> 'msg t
    (** [ol attrs children]

        Represents an ordered list of items.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/ol
    *)

    val p: 'msg attr list -> 'msg t list -> 'msg t
    (** [p attrs children]

        Represents a text paragraph.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/p
    *)

    val pre: 'msg attr list -> 'msg t list -> 'msg t
    (** [pre attrs children]

        Represents preformatted text which is to be presented exactly as written.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/pre
    *)

    val ul: 'msg attr list -> 'msg t list -> 'msg t
    (** [ul attrs children]

        Defines an unordered list of items.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/ul
    *)




    (** {1 Inline text semantics} *)

    val a: 'msg attr list -> 'msg t list -> 'msg t
    (** [a attrs children]

        Represents a hyperlink.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/a
    *)

    val abbr: 'msg attr list -> 'msg t list -> 'msg t
    (** [abbr attrs children]

        Represents an abbreviation or an acronym. The [title] attribute allows
        specifying the full expansion for the abbreviation.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/abbr
    *)

    val b: 'msg attr list -> 'msg t list -> 'msg t
    (** [b attrs children]

        Used to draw the reader's attention to the element's contents. This is
        not meant to indicate special importance. Use {!strong} for that instead.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/b
    *)

    val bdi: 'msg attr list -> 'msg t list -> 'msg t
    (** [bdi attrs children]

        Represents text that should be treated in isolation from its surrounding
        when it comes to text directionality. This allows embedding text
        snippets with a different or unknown directionality.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/bdi
    *)

    val bdo: 'msg attr list -> 'msg t list -> 'msg t
    (** [bdo attrs children]

        Overrides the current directionality of text using the [dir] attribute,
        so that the text within is rendered in a different direction.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/bdo
    *)

    val br: 'msg attr list -> 'msg t list -> 'msg t
    (** [br attrs children]

        Produces a line break.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/br
    *)

    val cite: 'msg attr list -> 'msg t list -> 'msg t
    (** [cite attrs children]

        Represents the title of a cited work.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/cite
    *)

    val code: 'msg attr list -> 'msg t list -> 'msg t
    (** [code attrs children]

        Represents a piece of computer code.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/code
    *)

    val data: 'msg attr list -> 'msg t list -> 'msg t
    (** [data attrs children]

        Links a piece of content with a machine-readable representation
        (specified in the [value] attribute).

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/data
    *)

    val dfn: 'msg attr list -> 'msg t list -> 'msg t
    (** [dfn attrs children]

        Represents a term that is defined in the surrounding element. If it has
        an [id] attribute, it can be linked to with an {!a} element.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/dfn
    *)

    val em: 'msg attr list -> 'msg t list -> 'msg t
    (** [em attrs children]

        Marks text as emphasized, e.g. to signal that a world should be
        stressed. Typically the text is rendered in italic.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/em
    *)

    val i: 'msg attr list -> 'msg t list -> 'msg t
    (** [i attrs children]

        Represents a range of text that is set off from the normal text.
        Possible reasons for that are that the text should be read in different
        voice or it is a technical term or a term from another language.
        Typically the text is rendered in italic.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/i
    *)

    val kbd: 'msg attr list -> 'msg t list -> 'msg t
    (** [kbd attrs children]

        Represents the textual representation of some user input, such as a
        keyboard shortcut.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/kbd
    *)

    val mark: 'msg attr list -> 'msg t list -> 'msg t
    (** [mark attrs children]

        Represents text that is marked or highlighted due to its relevance in
        the enclosing context.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/mark
    *)

    val q: 'msg attr list -> 'msg t list -> 'msg t
    (** [q attrs children]

        Represents a short inline quotation.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/q
    *)

    val rp: 'msg attr list -> 'msg t list -> 'msg t
    (** [rp attrs children]

        Used to provide fall-back parentheses for browsers that do not support
        the display of ruby annotations using the {!ruby} element.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/rp
    *)

    val rt: 'msg attr list -> 'msg t list -> 'msg t
    (** [rt attrs children]

        Specifies the ruby text component of a ruby annotation.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/rt
    *)

    val ruby: 'msg attr list -> 'msg t list -> 'msg t
    (** [ruby attrs children]

        Represents small annotations that are rendered above, below, or next
        to base text, usually used for showing the pronunciation of East Asian
        characters.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/ruby
    *)

    val s: 'msg attr list -> 'msg t list -> 'msg t
    (** [s attrs children]

        Represent text that is no longer relevant or no longer accurate.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/s
    *)

    val samp: 'msg attr list -> 'msg t list -> 'msg t
    (** [samp attrs children]

        Allows embedding the output of a computer program as inline text.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/samp
    *)

    val small: 'msg attr list -> 'msg t list -> 'msg t
    (** [small attrs children]

        Represents side-comments and small print, like copyright notices and
        legal text.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/small
    *)

    val span: 'msg attr list -> 'msg t list -> 'msg t
    (** [span attrs children]

        Represents a generic container for inline text. More specific elements
        such as {!em}, {!mark} or {!strong} should be used instead if their
        semantics apply to the content.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/span
    *)

    val strong: 'msg attr list -> 'msg t list -> 'msg t
    (** [strong attrs children]

        Represents important text. Typically it is rendered with a bold font.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/strong
    *)

    val sub: 'msg attr list -> 'msg t list -> 'msg t
    (** [sub attrs children]

        Defines text that should be displayed as subscript.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/sub
    *)

    val sup: 'msg attr list -> 'msg t list -> 'msg t
    (** [sup attrs children]

        Defines text that should be displayed as superscript.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/sup
    *)

    val time: 'msg attr list -> 'msg t list -> 'msg t
    (** [time attrs children]

        Represents a data and time value. The [datetime] attribute allows
        specifying this value in a machine-readable format.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/time
    *)

    val u: 'msg attr list -> 'msg t list -> 'msg t
    (** [u attrs children]

        Defines text that should be rendered with a non-textual annotation.
        Typically the text is rendered with an underline.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/u
    *)

    val var: 'msg attr list -> 'msg t list -> 'msg t
    (** [var attrs children]

        Represents the name of a variable in a mathematical expression or a
        programming context.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/var
    *)

    val wbr: 'msg attr list -> 'msg t list -> 'msg t
    (** [wbr attrs children]

        Marks a position within a word where the browser may optionally insert
        a line break. Can be used when the standard line-breaking rules don't
        yield the desired outcome.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/wbr
    *)




    (** {1 Image and multimedia} *)

    val area: 'msg attr list -> 'msg t list -> 'msg t
    (** [area attrs children]

        Defines an area inside an {{!map_}image map} that has predefined
        clickable areas.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/area
    *)

    val audio: 'msg attr list -> 'msg t list -> 'msg t
    (** [audio attrs children]

        Used to embed sound content in documents.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/audio
    *)

    val img: 'msg attr list -> 'msg t list -> 'msg t
    (** [img attrs children]

        Embeds an image into the document.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/img
    *)

    val map_: 'msg attr list -> 'msg t list -> 'msg t
    (** [map_ attrs children]

        Defines an image map which is an image with several clickable
        {{!area}areas}.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/map
    *)

    val track: 'msg attr list -> 'msg t list -> 'msg t
    (** [track attrs children]

        Specifies a timed text track for media elements like {!video} or
        {!audio}.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/track
    *)

    val video: 'msg attr list -> 'msg t list -> 'msg t
    (** [video attrs children]

        Embeds a media player which supports video playback into the document.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/video
    *)




    (** {1 Embedded content} *)

    val embed: 'msg attr list -> 'msg t list -> 'msg t
    (** [embed attrs children]

        Embeds external content, provided by an external application, into the
        document.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/embed
    *)

    val fencedframe: 'msg attr list -> 'msg t list -> 'msg t
    (** [fencedframe attrs children]

        Represents a nested browsing context, like {!iframe} but with more native
        privacy features built in.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/fencedframe
    *)

    val iframe: 'msg attr list -> 'msg t list -> 'msg t
    (** [iframe attrs children]

        Represents a nested browsing context, embedding another HTML page into
        the current one.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/iframe
    *)

    val object_: 'msg attr list -> 'msg t list -> 'msg t
    (** [object_ attrs children]

        Represents an external resource, which can be treated as an image, a
        nested browsing context, or a resource to be handled by a plugin.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/object
    *)

    val picture: 'msg attr list -> 'msg t list -> 'msg t
    (** [picture attrs children]

        Contains zero or more <source> elements and one <img> element to offer
        alternative versions of an image for different display/device scenarios.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/picture
    *)

    val source: 'msg attr list -> 'msg t list -> 'msg t
    (** [source attrs children]

        Allows offering media content for elements like {!video} or {!audio} in
        multiple file formats.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/source
    *)




    (** {1 Canvas} *)

    val canvas: 'msg attr list -> 'msg t list -> 'msg t
    (** [canvas attrs children]

        Allows drawing graphics and animations using the canvas API or the WebGL
        API.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/canvas
    *)




    (** {1 Demarcating edits} *)

    val del: 'msg attr list -> 'msg t list -> 'msg t
    (** [del attrs children]

        Represents a range of text that has been removed from the document.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/del
    *)

    val ins: 'msg attr list -> 'msg t list -> 'msg t
    (** [ins attrs children]

        Represents a range of text that has been added to the document.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/ins
    *)




    (** {1 Table content} *)

    val caption: 'msg attr list -> 'msg t list -> 'msg t
    (** [caption attrs children]

        Specifies the title of a table.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/caption
    *)

    val col: 'msg attr list -> 'msg t list -> 'msg t
    (** [col attrs children]

        Defines one or more columns in a column group.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/col
    *)

    val colgroup: 'msg attr list -> 'msg t list -> 'msg t
    (** [colgroup attrs children]

        Defines a group of columns within a table.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/colgroup
    *)

    val table: 'msg attr list -> 'msg t list -> 'msg t
    (** [table attrs children]

        Represents tabular data, that is data in two dimensions.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/table
    *)

    val tbody: 'msg attr list -> 'msg t list -> 'msg t
    (** [tobdy attrs children]

        Defines the set of table rows containing the table's main data.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/tobdy
    *)

    val td: 'msg attr list -> 'msg t list -> 'msg t
    (** [td attrs children]

        Defines a data cell of a table.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/td
    *)

    val tfoot: 'msg attr list -> 'msg t list -> 'msg t
    (** [tfoot attrs children]

        Defines the set of table rows, located at the bottom of the table,
        containing summaries about the table's columns.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/tfoot
    *)

    val th: 'msg attr list -> 'msg t list -> 'msg t
    (** [th attrs children]

        Defines a header cell for one or more data cells.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/th
    *)

    val thead: 'msg attr list -> 'msg t list -> 'msg t
    (** [thead attrs children]

        Defines the set of table rows, located at the top of the table,
        containing the titles of the table's columns.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/thead
    *)

    val tr: 'msg attr list -> 'msg t list -> 'msg t
    (** [tr attrs children]

        Defines a row of a table.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/tr
    *)




    (** {1 Forms} *)

    val button: 'msg attr list -> 'msg t list -> 'msg t
    (** [button attrs children]

        Defines a button.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/button
    *)

    val datalist: 'msg attr list -> 'msg t list -> 'msg t
    (** [datalist attrs children]

        Pre-defines a list of options for use in other input controls.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/datalist
    *)

    val fieldset: 'msg attr list -> 'msg t list -> 'msg t
    (** [fieldset attrs children]

        Used to group several controls and labels within a form.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/fieldset
    *)

    val form: 'msg attr list -> 'msg t list -> 'msg t
    (** [form attrs children]

        Represents a section containing user input controls and a submit button.
        Allows submitting user input to the server.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/form
    *)

    val input: 'msg attr list -> 'msg t list -> 'msg t
    (** [input attrs children]

        Allows a wide variety of user input, e.g. text, numbers, colors, date
        and time.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/input
    *)

    val label: 'msg attr list -> 'msg t list -> 'msg t
    (** [label attrs children]

        Specifies a caption for another element. This is often used to describe
        user input elements.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/label
    *)

    val legend: 'msg attr list -> 'msg t list -> 'msg t
    (** [legend attrs children]

        Represents a caption for the content of a fieldset.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/legend
    *)

    val meter: 'msg attr list -> 'msg t list -> 'msg t
    (** [meter attrs children]

        Represents either a scalar value or fractional value within a known
        range.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/meter
    *)

    val optgroup: 'msg attr list -> 'msg t list -> 'msg t
    (** [optgroup attrs children]

        Groups together multiple {!option} elements within a {!select} element.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/optgroup
    *)

    val option: 'msg attr list -> 'msg t list -> 'msg t
    (** [option attrs children]

        Define an item contained in a {!select}, an {!optgroup}, or a
        {!datalist} element.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/option
    *)

    val output: 'msg attr list -> 'msg t list -> 'msg t
    (** [output attrs children]

        A container for displaying the results of a calculation.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/output
    *)

    val progress: 'msg attr list -> 'msg t list -> 'msg t
    (** [progress attrs children]

        Displays an indicator showing the completion progress of a task.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/progress
    *)

    val select: 'msg attr list -> 'msg t list -> 'msg t
    (** [select attrs children]

        Represents a control that allows selecting out of multiple options.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/select
    *)

    val selectedcontent: 'msg attr list -> 'msg t list -> 'msg t
    (** [selectedcontent attrs children]

        Displays the content of the currently selected {!option} within a
        {!select} element.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/selectedcontent
    *)

    val textarea: 'msg attr list -> 'msg t list -> 'msg t
    (** [textarea attrs children]

        Represents a multi-line text editing control.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/textarea
    *)




    (** {1 Interactive elements} *)

    val details: 'msg attr list -> 'msg t list -> 'msg t
    (** [details attrs children]

        Represents a widget that the user can click to reveal additional
        information. A short summary of the contents must be provided in a
        {!summary} element.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/details
    *)

    val dialog: 'msg attr list -> 'msg t list -> 'msg t
    (** [dialog attrs children]

        Represents a dialog box or other interactive component, such as a
        dismissible alert, inspector, or subwindow.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/dialog
    *)

    val summary: 'msg attr list -> 'msg t list -> 'msg t
    (** [summary attrs children]

        Provides a summary of the information hidden in a {!details} element.
        Clicking the summary will toggle the "open" state of the details
        element.

        https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/summary
    *)




    (** {1 Reference Nodes}

        Reference nodes are nodes whose content is not controlled by the virtual
        dom. In the virtual dom the reference nodes are inserted by their name.

        The contents of reference nodes is controlled via
        {!val:Command.set_reference}.

        Reference nodes are persistent. Once referenced by {!val:reference} or
        initialized or updated by {!val:Command.set_reference} they exist. Once
        existing they can be modidfied by {!val:Command.set_reference}.

        The virtual dom can use them or not. They remain in existence.

        Reference nodes are a means to improve performance. In the following
        examples reference nodes might be useful:

        - Having an editor window in browser (e.g. CodeMirror): It does not make
        sense and is quite difficult to control an editor window by the virtual
        dome. It is better to create a reference node and let the internal state
        of the editor handled by some other meanss (e.g. CodeMirror code)

        - Spreadsheet with many cells: In a spreadsheet usully one cell is
        updated and some cells whose content depends on the edited cell have to
        be updated as well. Having a reference node for each cell makes it
        possible to update only the edited its dependent cells. Having all
        spreadsheet cells managed by the virtual dom requires a diffing of all
        cells. This can become quite slow if the spreadsheet is large.
     *)

    val reference: string -> 'msg t
    (**
        Insert a reference element into the dom.
    *)
end
