module type S =
sig
    type _ decoder
    type value
    type stop
    type prevent
    type file

    (** Attributes of Dom Elements.

        There are four types of attributes:

        - style attributes: Color, font size, etc.

        - property attributes: arbitrary javascript values as properties of the
        corresponding dom element.

        - attributes: string valued attributes to give the element an id, a
        classname, etc.

        - handler attributes: React to user interactions (mouse clicks, ...) on
        the html element.

        Sometimes the distinction between properties and attributes is quite
        subtle.  To the best of my knowledge each added attribute adds a
        property with the same name (except when the name is a javascript
        keyword like "class") to the html element. But not all properties even
        if it is a string value adds an attribute to the element.

     *)


    (** {1 Generic Interface}
     *)

    type 'msg t (** Type of an attribute potentially generating a message of
                    type ['msg]. *)


    val style: string -> string -> 'msg t
    (** [style key value] Add a style attribute to the html element.

        Examples:
        {[
            style "color"  "red"
            style "margin" "20px"
        ]}
     *)


    val class_: string -> 'msg t
    (** [class_ value] specifies CSS classes as a space-separated list.

        NOTE: Currently this should not be used in combination with
        {!class_list}. If both attributes are used, they overwrite each other.
    *)


    val class_list: (string * bool) list -> 'msg t
    (** [class_list classes]

        Allows conveniently specifying multiple classes. Each class comes with a
        condition and is only added to the element if that condition is [true].

        Example:
        {[
            let view_contact (c: contact): msg Html.t =
                div
                    [
                        class_list
                            [
                                ("contact", true);
                                ("contact-favourite", c.is_favourite);
                                ("contact-online", c.status = Online);
                                ("contact-offline", c.status = Offline);
                                ("contact-busy", c.status = Busy);
                            ]
                    ]
                    [
                        text c.full_name
                    ]
        ]}

        NOTE: Currently this should not be used in combination with {!class_}.
        If both attributes are used, they overwrite each other.
    *)


    val property: string -> value -> 'msg t
    (** [property key value] Add a javascript property to the html element. *)


    val attribute: string -> string -> 'msg t
    (** [attribute key value] Add an attribute to the html element.

        Examples:
        {[
            attribute "id" "my_element"
            attribute "class" "container"
        ]}
    *)


    val handler:
        string
        -> stop
        -> prevent
        -> 'msg decoder
        -> 'msg t
    (** [handler event_type stop_flag prevent_flag decoder]

        Attribute representing an event listener on an html element. The two
        flags decide if the event is propagated upwards in the dom tree and if
        default action (some events like clicking on an anchor element cause
        default actions in the browser) is prevented.

        The decoder decodes the javascript event object into a message of type
        ['msg].

        Starting from the event object information from the whole dom tree up to
        the root can be decode. Each event object has a target (which is the
        element on which it is fired). The target element has a tag name, can
        have various properties etc. For more details on event objects see the
        {{: https://developer.mozilla.org/en-US/docs/Web/API/Event} event api}.

        More information on {{!page-doc_event_handler} event handlers}.
     *)


    val map: ('a -> 'b) -> 'a t -> 'b t
    (** [map f a] Map an attribute creating messages of type ['a] to an
        attribute creating messages of type ['b].
    *)


    (** {1 Handler} *)

    val on: string -> 'msg decoder -> 'msg t
    (** [on event_type decoder]

        is equivalent to
        [handler event_type Event_flag.no_stop Event_flag.no_prevent decoder]
    *)

    val on_click: 'msg -> 'msg t
    (** [on_click m] produce the message [m] on mouse click. *)


    val on_mouseenter: 'msg -> 'msg t
    (** [on_mouseenter m] Produce the message [m] when the mouse enters the
        element.
     *)


    val on_mouseleave: 'msg -> 'msg t
    (** [on_mouseleave m] Produce the message [m] when the mouse leaves the
        element.
     *)


    val on_keydown: (string -> 'msg) -> 'msg t
    (** [on_keydown f]

        Produce the message [f key] on the keydown event with [key].
    *)

    val on_keyup: (string -> 'msg) -> 'msg t
    (** [on_keyup f]

        Produce the message [f key] on the keyup event with [key].
    *)

    val on_input: (string -> 'msg) -> 'msg t




    (** {1 Standard attributes} *)

    val accept: string -> 'msg t
    (** [accept value]

        Comma-separated list of file types the server accepts.

        Elements: {{!Html.form}<form>}, {{!Html.input}<input>}
    *)

    val accept_charset: string -> 'msg t
    (** [accept_charset value]

        The character set, which if provided must be "UTF-8".

        Elements: {{!Html.form}<form>}
    *)

    val accesskey: string -> 'msg t
    (** [accesskey value]

        Keyboard shortcut to activate or add focus to the element.

        Elements: all
    *)

    val action: string -> 'msg t
    (** [action value]

        The URL that processes the form submission.

        Elements: {{!Html.form}<form>}
    *)

    val allow: string -> 'msg t
    (** [allow value]

        Specifies a feature-policy for the iframe.

        Elements: {{!Html.iframe}<iframe>}
    *)

    val alpha: bool -> 'msg t
    (** [alpha value]

        Allow the user to select a color's opacity on a [type="color"] input.

        Elements: {{!Html.input}<input>}
    *)

    val alt: string -> 'msg t
    (** [alt value]

        Alternative text in case an image can't be displayed.

        Elements: {{!Html.area}<area>}, {{!Html.img}<img>},
        {{!Html.input}<input>}
    *)

    val autocapitalize: string -> 'msg t
    (** [autocapitalize value]

        Sets whether input is automatically capitalized when entered by user.

        Elements: all
    *)

    val autocomplete: string -> 'msg t
    (** [autocomplete value]

        Indicates whether controls in this form can have their values
        automatically completed by the browser.

        Elements: {{!Html.form}<form>}, {{!Html.input}<input>},
        {{!Html.select}<select>}, {{!Html.textarea}<textarea>}
    *)

    val autoplay: bool -> 'msg t
    (** [autoplay value]

        Whether audio or video should play as soon as possible.

        Elements: {{!Html.audio}<audio>}, {{!Html.video}<video>}
    *)

    val capture: string -> 'msg t
    (** [capture value]

        Specifies that file contents should be captured from a camera and/or
        microphone. Applies to [type="file"] input elements.

        Elements: {{!Html.input}<input>}
    *)

    val checked: bool -> 'msg t
    (** [checked value]

        Indicates whether the checkbox (a [type="checkbox"] input element)
        should be checked. This not only sets the initial value of the input
        element, but also allows overriding the value after the user edited it.

        Elements: {{!Html.input}<input>}
    *)

    val cite: string -> 'msg t
    (** [cite value]

        Contains a URI which points to the source of the quote or change.

        Elements: {{!Html.blockquote}<blockquote>}, {{!Html.del}<del>},
        {{!Html.ins}<ins>}, {{!Html.q}<q>}
    *)

    val colorspace: string -> 'msg t
    (** [colorspace value]

        Defines the color space that is used by a [type="color"] input.

        Elements: {{!Html.input}<input>}
    *)

    val cols: int -> 'msg t
    (** [cols value]

        Defines the number of columns in a textarea.

        Elements: {{!Html.textarea}<textarea>}
    *)

    val colspan: int -> 'msg t
    (** [colspan value]

        Defines the number of columns a cell should span.

        Elements: {{!Html.td}<td>}, {{!Html.th}<th>}
    *)

    val contenteditable: string -> 'msg t
    (** [contenteditable value]

        Indicates whether the element's content is editable.

        Elements: all
    *)

    val controls: bool -> 'msg t
    (** [controls value]

        Indicates whether the browser should show playback controls to the user.

        Elements: {{!Html.audio}<audio>}, {{!Html.video}<video>}
    *)

    val coords: string -> 'msg t
    (** [coords value]

        A set of values specifying the coordinates of the hot-spot region.

        Elements: {{!Html.area}<area>}
    *)

    val crossorigin: string -> 'msg t
    (** [crossorigin value]

        How the element handles cross-origin requests.

        Elements: {{!Html.audio}<audio>}, {{!Html.img}<img>},
        {{!Html.video}<video>}
    *)

    val csp: string -> 'msg t
    (** [csp value]

        Specifies the Content Security Policy that an embedded document must
        agree to enforce upon itself.

        Elements: {{!Html.iframe}<iframe>}
    *)

    val data: string -> 'msg t
    (** [data value]

        Specifies the URL of the resource.

        Elements: {{!Html.object_}<object>}
    *)

    val datetime: string -> 'msg t
    (** [datetime value]

        Indicates the date and time associated with the element.

        Elements: {{!Html.del}<del>}, {{!Html.ins}<ins>}, {{!Html.time}<time>}
    *)

    val decoding: string -> 'msg t
    (** [decoding value]

        Indicates the preferred method to decode the image.

        Elements: {{!Html.iframe}<iframe>}
    *)

    val default: bool -> 'msg t
    (** [default value]

        Indicates that the track should be enabled unless the user's preferences
        indicate something different.

        Elements: {{!Html.track}<track>}
    *)

    val dir: string -> 'msg t
    (** [dir value]

        Defines the text direction.

        Elements: all
    *)

    val dirname: string -> 'msg t
    (** [dirname value]

        Specifies the name of a field that will contain the automatically
        determined text direction, when a form is submitted.

        Elements: {{!Html.input}<input>}, {{!Html.textarea}<textarea>}
    *)

    val disabled: bool -> 'msg t
    (** [disabled value]

        Indicates whether the user can interact with the element.

        Elements: {{!Html.button}<button>}, {{!Html.fieldset}<fieldset>},
        {{!Html.input}<input>}, {{!Html.optgroup}<optgroup>},
        {{!Html.option}<option>}, {{!Html.select}<select>},
        {{!Html.textarea}<textarea>}
    *)

    val download: string -> 'msg t
    (** [download value]

        Indicates that the hyperlink is to be used for downloading a resource.

        Elements: {{!Html.a}<a>}, {{!Html.area}<area>}
    *)

    val draggable: string -> 'msg t
    (** [draggable value]

        Defines whether the element can be dragged.

        Elements: all
    *)

    val enctype: string -> 'msg t
    (** [enctype value]

        Defines the content type of the form data when the method is POST.

        Elements: {{!Html.form}<form>}
    *)

    val enterkeyhint: string -> 'msg t
    (** [enterkeyhint value]

        Specifies what action label (or icon) to present for the enter key on
        virtual keyboards.

        Elements: {{!Html.textarea}<textarea>}, elements with
        ["contenteditable"] set
    *)

    val elementtiming: string -> 'msg t
    (** [elementtiming value]

        Indicates that an element is flagged for tracking by
        [PerformanceObserver] objects.

        Elements: {{!Html.img}<img>}, {{!Html.img}<video>}, elements containing
        text nodes such as {{!Html.p}<p>}
    *)

    val for_: string -> 'msg t
    (** [for_ value]

        Describes elements which belongs to this one (space-separated list of
        IDs).

        Elements: {{!Html.a}<a>}
    *)

    val form: string -> 'msg t
    (** [form value]

        Indicates the form that is the owner of the element.

        Elements: {{!Html.button}<button>}, {{!Html.fieldset}<fieldset>},
        {{!Html.input}<input>}, {{!Html.object_}<object>},
        {{!Html.output}<output>}, {{!Html.select}<select>},
        {{!Html.textarea}<textarea>}
    *)

    val formaction: string -> 'msg t
    (** [formaction value]

        Sets the action of the element. Overrides the form's {!action}
        attribute.

        Elements: {{!Html.input}<input>}, {{!Html.button}<button>}
    *)

    val formenctype: string -> 'msg t
    (** [formenctype value]

        Sets the encoding type to use during form submission. Overrides the
        form's {!enctype} attribute.

        Elements: {{!Html.input}<input>}, {{!Html.button}<button>}
    *)

    val formmethod: string -> 'msg t
    (** [formmethod value]

        Sets the submission method to use during form submission. Overrides the
        form's {{!method_}method} attribute.

        Elements: {{!Html.input}<input>}, {{!Html.button}<button>}
    *)

    val formnovalidate: bool -> 'msg t
    (** [formnovalidate value]

        Specifies that the form should not be validated when it is submitted.
        Overrides the form's {!novalidate} attribute.

        Elements: {{!Html.input}<input>}, {{!Html.button}<button>}
    *)

    val formtarget: string -> 'msg t
    (** [formtarget value]

        Indicates where to display the response after submitting the form.
        Overrides the form's {!target} attribute.

        Elements: {{!Html.input}<input>}, {{!Html.button}<button>}
    *)

    val headers: string -> 'msg t
    (** [headers value]

        IDs of the table header elements corresponding to this element
        (space-separated list).

        Elements: {{!Html.td}<td>}, {{!Html.th}<th>}
    *)

    val height: int -> 'msg t
    (** [height value]

        Specifies the height of the element.

        Elements: {{!Html.val-canvas}<canvas>}, {{!Html.embed}<embed>},
        {{!Html.iframe}<iframe>}, {{!Html.img}<img>}, {{!Html.input}<input>},
        {{!Html.object_}<object>}, {{!Html.video}<video>}
    *)

    val hidden: string -> 'msg t
    (** [hidden value]

        Prevents rendering the element.

        Elements: all
    *)

    val high: float -> 'msg t
    (** [high value]

        Indicates the lower bound of the upper range.

        Elements: {{!Html.meter}<meter>}
    *)

    val href: string -> 'msg t
    (** [href value]

        The URL of a linked resource.

        Elements: {{!Html.a}<a>}, {{!Html.area}<area>}
    *)

    val hreflang: string -> 'msg t
    (** [hreflang value]

        Specifies the language of the linked resource.

        Elements: {{!Html.a}<a>}
    *)

    val id: string -> 'msg t
    (** [id value]

        A unique value identifying the element.

        Elements: all
    *)

    val inputmode: string -> 'msg t
    (** [inputmode value]

        Provides a hint about the type of data a user can enter.

        Elements: {{!Html.textarea}<textarea>}, elements with
        ["contenteditable"] set
    *)

    val ismap: bool -> 'msg t
    (** [ismap value]

        Indicates that the image is part of a server-side image map.

        Elements: {{!Html.img}<img>}
    *)

    val itemprop: string -> 'msg t
    (** [itemprop value]

        Allows adding properties to an item.

        Elements: all
    *)

    val kind: string -> 'msg t
    (** [kind value]

        Specifies the kind of text track.

        Elements: {{!Html.track}<track>}
    *)

    val label: string -> 'msg t
    (** [label value]

        Specifies a user-readable title of the element.

        Elements: {{!Html.optgroup}<optgroup>}, {{!Html.option}<option>},
        {{!Html.track}<track>}
    *)

    val lang: string -> 'msg t
    (** [lang value]

        Defines the language used in the element.

        Elements: all
    *)

    val loading: string -> 'msg t
    (** [loading value]

        Indicates if the element should be loaded lazily or immediately.

        Elements: {{!Html.img}<img>}, {{!Html.iframe}<iframe>}
    *)

    val list: string -> 'msg t
    (** [list value]

        Specifies the ID of a datalist element of pre-defined options.

        Elements: {{!Html.input}<input>}
    *)

    val loop: bool -> 'msg t
    (** [loop value]

        Indicates whether the media should start playing from the start when
        it's finished.

        Elements: {{!Html.audio}<audio>}, {{!Html.video}<video>}
    *)

    val low: float -> 'msg t
    (** [low value]

        Indicates the upper bound of the lower range.

        Elements: {{!Html.meter}<meter>}
    *)

    val max: string -> 'msg t
    (** [max value]

        Indicates the maximum value allowed.

        Elements: {{!Html.input}<input>}, {{!Html.meter}<meter>}, {{!Html.progress}<progress>}
    *)

    val maxlength: int -> 'msg t
    (** [maxlength value]

        Defines the maximum number of characters allowed in the element.

        Elements: {{!Html.input}<input>}, {{!Html.textarea}<textarea>}
    *)

    val minlength: int -> 'msg t
    (** [minlength value]

        Defines the minimum number of characters allowed in the element.

        Elements: {{!Html.input}<input>}, {{!Html.textarea}<textarea>}
    *)

    val media: string -> 'msg t
    (** [media value]

        Specifies a hint of the media for which the linked resource was
        designed.

        Elements: {{!Html.a}<a>}, {{!Html.area}<area>}, {{!Html.source}<source>}
    *)

    val method_: string -> 'msg t
    (** [method value]

        Defines which HTTP method to use when submitting the form.

        Elements: {{!Html.form}<form>}
    *)

    val min: string -> 'msg t
    (** [min value]

        Indicates the minimum value allowed.

        Elements: {{!Html.input}<input>}, {{!Html.meter}<meter>}
    *)

    val multiple: bool -> 'msg t
    (** [multiple value]

        Indicates whether multiple values can be entered in an input of the type
        [email] or [file].

        Elements: {{!Html.input}<input>}, {{!Html.select}<select>}
    *)

    val muted: bool -> 'msg t
    (** [muted value]

        Indicates whether the audio will be initially silenced on page load.

        Elements: {{!Html.audio}<audio>}, {{!Html.video}<video>}
    *)

    val name: string -> 'msg t
    (** [name value]

        Name of the element.

        Elements: {{!Html.button}<button>}, {{!Html.form}<form>},
        {{!Html.fieldset}<fieldset>}, {{!Html.iframe}<iframe>},
        {{!Html.input}<input>}, {{!Html.object_}<object>},
        {{!Html.output}<output>}, {{!Html.select}<select>},
        {{!Html.textarea}<textarea>}, {{!Html.map}<map>}
    *)

    val novalidate: bool -> 'msg t
    (** [novalidate value]

        Indicates that the form shouldn't be validated when submitted.

        Elements: {{!Html.form}<form>}
    *)

    val open_: bool -> 'msg t
    (** [open_ value]

        Indicates whether the contents are currently visible.

        Elements: {{!Html.details}<details>}, {{!Html.dialog}<dialog>}
    *)

    val optimum: float -> 'msg t
    (** [optimum value]

        Indicates the optimal value.

        Elements: {{!Html.meter}<meter>}
    *)

    val pattern: string -> 'msg t
    (** [pattern value]

        Defines a regular expression which the element's value will be validated
        against.

        Elements: {{!Html.input}<input>}
    *)

    val ping: string -> 'msg t
    (** [ping value]

        Specifies a space-separated list of URLs to be notified if a user
        follows the hyperlink.

        Elements: {{!Html.a}<a>}, {{!Html.area}<area>}
    *)

    val placeholder: string -> 'msg t
    (** [placeholder value]

        Provides a hint to the user of what can be entered in the field.

        Elements: {{!Html.input}<input>}, {{!Html.textarea}<textarea>}
    *)

    val playsinline: bool -> 'msg t
    (** [playsinline value]

        Indicates that the video is to be played "inline"; that is, within the
        element's playback area.

        Elements: {{!Html.video}<video>}
    *)

    val poster: string -> 'msg t
    (** [poster value]

        A URL indicating a poster frame to show until the user plays or seeks.

        Elements: {{!Html.video}<video>}
    *)

    val preload: string -> 'msg t
    (** [preload value]

        Indicates whether the whole resource, or parts of it should be
        preloaded.

        Elements: {{!Html.audio}<audio>}, {{!Html.video}<video>}
    *)

    val readonly: bool -> 'msg t
    (** [readonly value]

        Indicates whether the element can be edited.

        Elements: {{!Html.input}<input>}, {{!Html.textarea}<textarea>}
    *)

    val referrerpolicy: string -> 'msg t
    (** [referrerpolicy value]

        Specifies which referrer is sent when fetching the resource.

        Elements: {{!Html.a}<a>}, {{!Html.area}<area>},
        {{!Html.iframe}<iframe>}, {{!Html.img}<img>}
    *)

    val rel: string -> 'msg t
    (** [rel value]

        Specifies the relationship of the target object to the link object.

        Elements: {{!Html.a}<a>}, {{!Html.area}<area>}
    *)

    val required: bool -> 'msg t
    (** [required value]

        Indicates whether this element is required to fill out or not.

        Elements: {{!Html.input}<input>}, {{!Html.select}<select>},
        {{!Html.textarea}<textarea>}
    *)

    val reversed: bool -> 'msg t
    (** [reversed value]

        Indicates whether the list should be displayed in a descending order
        instead of an ascending order.

        Elements: {{!Html.ol}<ol>}
    *)

    val role: string -> 'msg t
    (** [role value]

        Defines an explicit role for an element for use by assistive
        technologies.

        Elements: all
    *)

    val rows: int -> 'msg t
    (** [rows value]

        Defines the number of rows in a text area.

        Elements: {{!Html.textarea}<textarea>}
    *)

    val rowspan: int -> 'msg t
    (** [rowspan value]

        Defines the number of rows a table cell should span over.

        Elements: {{!Html.td}<td>}, {{!Html.th}<th>}
    *)

    val sandbox: string -> 'msg t
    (** [sandbox value]

        Stops a document loaded in an iframe from using certain features.

        Elements: {{!Html.iframe}<iframe>}
    *)

    val scope: string -> 'msg t
    (** [scope value]

        Defines which cells the header element relates to.

        Elements: {{!Html.th}<th>}
    *)

    val selected: bool -> 'msg t
    (** [selected value]

        Defines a value which will be selected on page load.

        Elements: {{!Html.option}<option>}
    *)

    val shape: string -> 'msg t
    (** [shape value]

        The shape of a region in an image map.

        Elements: {{!Html.a}<a>}, {{!Html.area}<area>}
    *)

    val size: int -> 'msg t
    (** [size value]

        Defines the width of the element (in pixels or characters depending on
        the {{!type_}type}).

        Elements: {{!Html.input}<input>}, {{!Html.select}<select>}
    *)

    val sizes: string -> 'msg t
    (** [sizes value]

        One or more media conditions (specifying the intended viewport) or
        source-size values (specifying the intended display size of the image).
        A comma-separated list.

        Elements: {{!Html.img}<img>}, {{!Html.source}<source>}
    *)

    val span: int -> 'msg t
    (** [span value]

        The number of columns the element should span.

        Elements: {{!Html.col}<col>}, {{!Html.colgroup}<colgroup>}
    *)

    val spellcheck: string -> 'msg t
    (** [spellcheck value]

        Indicates whether spell checking is allowed for the element.

        Elements: all
    *)

    val src: string -> 'msg t
    (** [src value]

        The URL of the embeddable content.

        Elements: {{!Html.audio}<audio>}, {{!Html.embed}<embed>},
        {{!Html.iframe}<iframe>}, {{!Html.img}<img>}, {{!Html.input}<input>},
        {{!Html.source}<source>}, {{!Html.track}<track>}, {{!Html.video}<video>}
    *)

    val srcdoc: string -> 'msg t
    (** [srcdoc value]

        Specifies inline HTML which will be embedded in the document. Overrides
        the {!src} attribute.

        Elements: {{!Html.iframe}<iframe>}
    *)

    val srclang: string -> 'msg t
    (** [srclang value]

        Language of the track text data.

        Elements: {{!Html.track}<track>}
    *)

    val srcset: string -> 'msg t
    (** [srcset value]

        One or more responsive image candidates (comma-separated list).

        Elements: {{!Html.img}<img>}, {{!Html.source}<source>}
    *)

    val start: int -> 'msg t
    (** [start value]

        Defines the number a numbered list should start counting at.

        Elements: {{!Html.ol}<ol>}
    *)

    val step: float -> 'msg t
    (** [step value]

        Defines the interval between the discrete values the user can enter.

        Elements: {{!Html.input}<input>}
    *)

    val tabindex: int -> 'msg t
    (** [tabindex value]

        Overrides the browser's default tab order and follows the one specified
        instead.

        Elements: all
    *)

    val target: string -> 'msg t
    (** [target value]

        Specifies where to display the requested document.

        Elements: {{!Html.a}<a>}, {{!Html.area}<area>}, {{!Html.form}<form>}
    *)

    val title: string -> 'msg t
    (** [title value]

        Text to be displayed in a tooltip when hovering over the element.

        Elements: all
    *)

    val translate: string -> 'msg t
    (** [translate value]

        Specifies whether the elements attribute values and text contents should
        be translated by automatic translation systems.

        Elements: all
    *)

    val type_: string -> 'msg t
    (** [type_ value]

        Defines the type of the element.

        Elements: {{!Html.button}<button>}, {{!Html.input}<input>},
        {{!Html.embed}<embed>}, {{!Html.object_}<object>}, {{!Html.ol}<ol>},
        {{!Html.source}<source>}, {{!Html.menu}<menu>}
    *)

    val usemap: string -> 'msg t
    (** [usemap value]

        The partial URL (starting with #) of an image map associated with the
        element.

        Elements: {{!Html.img}<img>}
    *)

    val value: string -> 'msg t
    (** [value value]

        Defines the value of the element. This not only sets the initial value
        of the input element, but also allows overriding the value after the
        user edited it.

        Elements: {{!Html.input}<input>}
    *)

    val width: int -> 'msg t
    (** [width value]

        Specifies the width of the element.

        Elements: {{!Html.val-canvas}<canvas>}, {{!Html.embed}<embed>},
        {{!Html.iframe}<iframe>}, {{!Html.img}<img>}, {{!Html.input}<input>},
        {{!Html.object_}<object>}, {{!Html.video}<video>}
    *)

    val wrap: string -> 'msg t
    (** [wrap value]

        Indicates whether the text should be wrapped.

        Elements: {{!Html.textarea}<textarea>}
    *)





    (** {1 Common style attributes} *)

    val font_size: string -> 'msg t
    (** Example [font_size "20px"]

        Abbreviates [style "font-size" "20px"].
    *)


    val color: string -> 'msg t
    (** Example [color "red"]

        Abbreviates [style "color" "red"].
    *)


    val background_color: string -> 'msg t
    (** Example [background_color "powderblue"]

        Abbreviates [style "background-color" "powderblue"].
    *)



    (** {1 Margin, border, padding and content}

        {v

               +--------------------------------+
               |         margin                 |
               |  +----border-----------------+ |
               |  |      padding              | |
               |  |   +---------------------+ | |
               |  |   |                     | | |
               |  |   |                     | | |
               |  |   |      content        | | |
               |  |   |                     | | |
               |  |   +---------------------+ | |
               |  +---------------------------+ |
               |                                |
               +--------------------------------+
        v}
     *)


    val margin: string -> 'msg t
    (** Examples

        {[
            margin "25px"
            margin "25px 50px"              top/bottom 25px, left/right 50px
            margin "25px 50px 75px 100px"   top, right, bottom, left
        ]}

        [margin str] abbreviates [style "margin" str]
    *)


    val padding: string -> 'msg t
    (** Examples

        {[
            padding "25px"
            padding "25px 50px"              top/bottom 25px, left/right 50px
            padding "25px 50px 75px 100px"   top, right, bottom, left
        ]}

        [padding str] abbreviates [style "padding" str]
    *)


    val border_style: string -> 'msg t
    (** Examples

        {[
            border_style "solid"
            border_style "dotted"
            border_style "dashed"
        ]}

        [border_style str] abbreviates [style "border-style" str]
    *)

    val border_width: string -> 'msg t
    (** Examples

        {[
            border_width "3px"
            border_width "thick"
            border_width "medium"
        ]}
    *)

    val border_color: string -> 'msg t
    (** Example

        {[
            border_color "red"
        ]}
    *)
end
