module type NAV =
sig
    (* Navigation-related types *)

    type url

    (** When the user clicks a link in an {!application}, the URL is not changed
        directly. Instead a {!url_request} is passed to the application's
        [on_url_request] handler. The resulting message must be handled in the
        application's [update] function.

        This mechanism allows the application to prepare for the URL change,
        e.g. persist important data on the server or in local storage.

        A [url_request] can be either

        - an {!Internal} URL with the same origin as the application. The user
          is about to change pages within the application.
          {{!Command.push_url}push_url} should be used to perform the URL
          change.

        - an {!External} URL with a different origin than the application.
          The user is about to be redirected to an external site.
          {{!Command.load}load} should be used to continue.

        NOTE: The origin consists of the [protocol], the [host], and the [port]
        of the URL. If any of those changes, the new URL is seen as an
        {!External} URL. *)
    type url_request = Internal of url | External of string


    type 'm key
    (** This navigation key is handed to the {!application} through its [init]
        function. It is needed for the navigation commands
        {{!Command.push_url}push_url},
        {{!Command.replace_url}replace_url}, {{!Command.back}back} and
        {{!Command.forward}forward}.

        This means that those commands are only available in a full
        {!application} which controls the whole browser window and not in other
        kinds of programs that only control a part of it such as {!element}.
        This restriction exists to prevent subtle synchronization bugs
        related to URL changes.

        For example, when part of the website is controlled by an {!element}
        and another part is controlled by a [React] component and one of them
        changes the URL, the other needs to be informed about it. The navigation
        key restriction forces the programmer to implement URL changes and a
        synchronization mechanism outside of both components.

        The same restriction exists in [elm/browser]. See
        {{:https://github.com/elm/browser/blob/1.0.2/notes/navigation-in-elements.md}
        this document} for a more detailed explanation. *)
end
