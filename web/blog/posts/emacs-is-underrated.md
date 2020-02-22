TIP: Hello, and thanks for offering to review my draft! This post
intends to convey to people what the point of Emacs is. Not to convert
them to use it, but at least with opening their minds to the
possibility that it might contain valuable things. I don't know if I'm
on track in the right direction, and your input will help me figure it
out. Thanks!

TODO(tazjin): Restructure sections: Intro -> Introspectability (and
story) -> text-based UIs (which lead to fluidity, muscle memory across
programs and "translatability" of workflows) -> Outro. It needs more
flow!

TODO(tazjin): Highlight more that it's not about editing: People can
derive useful things from Emacs by just using magit/org/notmuch/etc.!

TODO(tazjin): Note that there's value in trying Emacs even if people
don't end up using it, similar to how learning languages like Lisp or
Haskell helps grow as a programmer even without using them day-to-day.

*Real post starts below!*

---------

There are two kinds of people: Those who use Emacs, and those who
think it is a text editor. This post is aimed at those in the second
category.

Emacs is the most critical piece of software I run. My [Emacs
configuration][emacs-config] has steadily evolved for almost a decade.
Emacs is my window manager, mail client, terminal, git client,
information management system and - perhaps unsurprisingly - text
editor.

Before going into why I chose to invest so much into this program,
follow me along on a little thought experiment:

----------

Lets say you use a proprietary spreadsheet program. You find that
there are features in it that *almost, but not quite* do what you
want.

What can you do? You can file a feature request to the company that
makes it and hope they listen, but for the likes of Apple and
Microsoft chances are they won't and there is nothing you can do.

Let's say you are also running an open-source program for image
manipulation. You again find that some of its features are subtly
different from what you would want them to do.

Things look a bit different this time - after all, the program is
open-source! You can go and fetch its source code, figure out its
internal structure and wrangle various layers of code into submission
until you find the piece that implements the functionality you want to
change. If you know the language it is written in; you can modify the
feature.

Now all that's left is figuring out its build system[^1], building and
installing it and moving over to the new version.

Realistically you are not going to do this much in the real world. The
friction to contributing to projects, especially complex ones, is
often quite high. For minor inconveniences, you might often find
yourself just shrugging and working around them.

What if it didn't have to be this way?

-------------

One of the core properties of Emacs is that it is *introspective* and
*self-documenting*.

For example: A few years ago, I had just switched over to using
[EXWM][], the Emacs X Window Manager. To launch applications I was
using an Emacs program called Helm that let me select installed
programs interactively and press <kbd>RET</kbd> to execute them.

This was very useful - until I discovered that if I tried to open a
second terminal window, it would display an error:

    Error: urxvt is already running

Had this been dmenu, I might have had to go through the whole process
described above to fix the issue. But it wasn't dmenu - it was an
Emacs program, and I did the following things:

1. I pressed <kbd>C-h k</kbd>[^2] (which means "please tell me what
   the following key does"), followed by <kbd>s-d</kbd> (which was my
   keybinding for launching programs).

2. Emacs displayed a new buffer saying, roughly:

   ```
   s-d runs the command helm-run-external-command (found in global-map),
   which is an interactive autoloaded compiled Lisp function in
   ‘.../helm-external.el’.

   It is bound to s-d.
   ```

   I clicked on the filename.

3. Emacs opened the file and jumped to the definition of
   `helm-run-external-command`. After a few seconds of reading through
   the code, I found this snippet:

   ```lisp
   (if (get-process proc)
       (if helm-raise-command
           (shell-command  (format helm-raise-command real-com))
         (error "Error: %s is already running" real-com))
     ;; ... the actual code to launch programs followed below ...
     )
   ```

4. I deleted the outer if-expression which implemented the behaviour I
   didn't want, pressed <kbd>C-M-x</kbd> to reload the code and saved
   the file.

The whole process took maybe a minute, and the problem was now gone.

Emacs isn't just "open-source", it actively encourages the user to
modify it, discover what to modify and experiment while it is running.

In some sense it is like the experience of the old Lisp machines, a
paradigm that we have completely forgotten.

---------------

Circling back to my opening statement: If Emacs is not a text editor,
then what *is* it?

The Emacs website says this:

> [Emacs] is an interpreter for Emacs Lisp, a dialect of the Lisp
> programming language with extensions to support text editing

The core of Emacs implements the language and the functionality needed
to evaluate and run it, as well as various primitives for user
interface construction such as buffers, windows and frames.

Every other feature of Emacs is implemented *in Emacs Lisp*.

The Emacs distribution ships with rudimentary text editing
functionality (and some language-specific support for the most popular
languages), but it also brings with it two IRC clients, a Tetris
implementation, a text-mode web browser, [org-mode][] and many other
tools.

Outside of the core distribution there is a myriad of available
programs for Emacs: [magit][] (the famous git porcelain), text-based
[HTTP clients][], even interactive [Kubernetes frontends][k8s].

What all of these tools have in common is that they use text-based
user interfaces (UI elements like images are used only sparingly in
Emacs), and that they can be introspected and composed like everything
else in Emacs.

If magit does not expose a git flag I need, it's trivial to add. If I
want a keybinding to jump from a buffer showing me a Kubernetes pod to
a magit buffer for the source code of the container, it only takes a
few lines of Emacs Lisp to implement.

As proficiency with Emacs Lisp ramps up, the environment becomes
malleable like clay and evolves along with the user's taste and needs.
Muscle memory learned for one program translates seamlessly to others,
and the overall effect is an improvement in *workflow fluidity* that
is difficult to overstate.

Also, workflows based on Emacs are *stable*. Moving my window
management to Emacs has meant that I'm not subject to the whim of some
third-party developer changing my window layouting features (as they
often do on MacOS).

To illustrate this: Emacs has development history back to the 1970s,
continuous git history that survived multiple VCS migrations [since
1985][first-commit] (that's 22 years before git itself was released!)
and there is code[^3] implementing interactive functionality that has
survived unmodified in Emacs *since then*.

---------------

Now, what is the point of this post?

I decided to write this after a recent [tweet][] by @IanColdwater (in
the context of todo-management apps):

> The fact that it's 2020 and the most viable answer to this appears
> to be Emacs might be the saddest thing I've ever heard

What bothers me is that people see this as *sad*. Emacs being around
for this long and still being unparalleled for many of the UX
paradigms implemented by its programs is, in my book, incredible - and
not sad.

How many other paradigms have survived this long? How many other tools
still have fervent followers, amazing [developer tooling][] and a
[vibrant ecosystem][] at this age?

Steve Yegge [said it best][babel][^5]: Emacs has the Quality Without a
Name.

What I wish you, the reader, should take away from this post is the
following:

TODO(tazjin): Figure out what people should take away from this post.
I need to sleep on it. It's something about not dismissing tools just
because of their age, urging them to explore paradigms that might seem
unfamiliar and so on. Ideas welcome.

---------------

[^1]: Wouldn't it be a joy if every project just used Nix? I digress ...
[^2]: These are keyboard shortcuts written in [Emacs Key Notation][ekn].
[^3]: For example, [functionality for online memes][studly] that
    wouldn't be invented for decades to come!
[^4]: ... and some things wrong, but that is an issue for a separate post!
[^5]: And I really *do* urge you to read that post's section on Emacs.

[emacs-config]: https://git.tazj.in/tree/tools/emacs
[EXWM]: https://github.com/ch11ng/exwm
[helm]: https://github.com/emacs-helm/helm
[ekn]: https://www.gnu.org/software/emacs/manual/html_node/efaq/Basic-keys.html
[org-mode]: https://orgmode.org/
[magit]: https://magit.vc
[HTTP clients]: https://github.com/pashky/restclient.el
[k8s]: https://github.com/jypma/kubectl
[first-commit]: http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=ce5584125c44a1a2fbb46e810459c50b227a95e2
[studly]: http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=47bdd84a0a9d20aab934482a64b84d0db63e7532
[tweet]: https://twitter.com/IanColdwater/status/1220824466525229056
[developer tooling]: https://github.com/alphapapa/emacs-package-dev-handbook
[vibrant ecosystem]: https://github.com/emacs-tw/awesome-emacs
[babel]: https://sites.google.com/site/steveyegge2/tour-de-babel#TOC-Lisp
