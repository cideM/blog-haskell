---
title: Editors
date: "2018-09-10"
---

In this post I will compare Visual Studio Code and Neovim, my two primary
editors. The comparison will include an arbitrary set of plugins for each
editor, depending on my needs and preferences. I will skip areas that don't
concern me and all my opinions are influenced by the languages and environments
in which I work.

As a professional front end developer I write JS for at least 8 hours every
day. My free time is spent in Haskell and Rust (~80/20). If one or both editors
suck for your \$LANGUAGE then that might not be accounted for in this post.

## Editing

Neovim is unrivalled at editing text. Thanks to both its built-in capabilities
and plugins like `vim-surround` and `targets` it's trivial to change text
within certain delimiters, or change the delimiters themselves. No VSC plugin
comes even close and the keyboard shortcuts and functions VSC offers pale in
comparison to even just the built-in stuff.

There is one exception though: multi-cursor support. I think that VSC's
multi-cursor implementation is in 75% of cases more intuitive and faster than
neovim macros. There are other cases in which multiple cursors simply won't do the
trick, where recorded macros really shine, but those are rare. I think that
Kakoune strikes the perfect balance between modal editing and giving me
immediate visual feedback. Still, Neovim runs circles around VSC at editing
text.

## Linting

THE most disappointing topic for me, it's 2019 after all! The JS developer in
me is perfectly happy with VSC. Why? Because the ESlint plugin is maintained by
Microsoft and it's a posterchild for language server plugins. Every other
editor out there that just manually invokes Eslint will be orders of magnitude
slower than VSC. At work we have a somewhat involved Webpack setup which
results in ESlint taking a long time to run -- except in VSC.

VSC currently has mediocre support for formatting error popups. That means if
your error messages are oneliners (as is the case for JS), it's totally fine.
But look at Haskell or the ASCII-art powered errors in Rust and you're losing
important information. Now neovim doesn't exactly solve this problem although
ALE with `:ALEDetail` gives error messages a little bit more breathing room.
Ultimately though I almost always resort to a terminal where I just run the
linter/compiler myself (think `ghcid`).

When it comes to linter support ALE generally has you covered and especially
for Haskell it offers more options than the individual VSC plugins.

At the end of the day, all editors suck equally in this regard and the terminal
reigns supreme. All hail TUIs!

## Project Navigation

VSC uses ripgrep. My neovim uses ripgrep. End of story?

Not quite. VSC has the upper hand in most cases since it handles escaping and
quotes in searches better than `:grep` in neovim. Its preview of search and
replace is also nicer than what you generally get in neovim (although
the `traces.vim` plugin helps).

If your search needs are really complicated then being able to pass options
directly through to ripgrep will put neovim ahead... eventually. It's kind of
the same story as with multiple cursors. I feel like VSC just nails the
"excellent in 95% of cases" thing with multiple cursors and search.

Apart from that though, neovim is generally more pleasant. The jumplist is more
accurate, slash search is faster than ctrl+f search, and with plugins like
`vim-qf` navigating search results (or whatever you put in the location and
quick fix list) is so fast that anyone looking over your shoulder won't be able
to keep up.

For languages where VSC has good support its symbol search is nice but honestly
in many cases I'm at the target location with just slash search/grep while VSC
hasn't even indexed the code base -- and it seems to do that everytime I open a
new project/switch projects.

## Speed

[Insert Electron trashing]

Honestly? I think VSC has the upper hand here. Neovim only reaches its full
potential when its running the terminal. That's how you inherit years and years
of cruft. Weird escape codes, true color, 256 color, 210391029 color,
underline, italics, terminal multiplexers, they all add their own little
complexities and quirks. At the end, yes of course Neovim uses way less
resources and if you use something like Alacritty everything's generally fast
but VSCs UI is incredibly snappy. I feel like the overall visual design and UI
snappiness of VSC beats neovim.

At the end of the day both editors are fast enough for my needs.

## UI

I love that I can use the same set of movements on every buffer, regardless if
it's running a terminal or code. That's beautiful. And VSC doesn't have that.
So yeah, not much of a competition here.

## Auto suggest

Neovim's buffer based completion runs circles around VSCs buffer based
completion but if a language is supported in VSC then obviously your completion
and suggestion experience will be much, much nicer than in Neovim. The rise of
language server plugins will hopefully shift that balance in favor of Neovim
but right now, if intelligent suggestions are you #1 thing, then Neovim might
not always be unicorns and rainbows.

## Git

There's one Git editor integration that I consider truly impressive and that's
in the IntelliJ editors. VSC has the GitLens plugin which is impressive for its
feature set, download count and supporter count. And yet, it seems to not have
anything I actually use. I love the CLI and `git add -p` and `git diff`. It's
exactly what I want. I do not need anything else for those use cases.
`vim-fugitive` then adds some other niceties. The most recent patch added the
`=` shortcut to expand the diff in the `:Gstatus` view. That doesn't mean much
if you don't use Neovim or that plugins so suffice it to say that viewing the
diff of your current changes is just like 2 key presses away (I didn't count).
One command I use a lot is showing me the version of the current file from
another branch.

There's one thing where Neovim not only drops the ball but fails to an epic
extent: merge conflicts. I'd rather pay 100USD for Sublime Merge or keep VSC
installed _just for resolving merge conflicts_. I think Neovim just isn't the
right environment for that (or rather TUIs aren't).

Bottom line: the Git plugins for VSC somehow manage to not have any features I
use. So the shell together with `fugitive` and `gv.vim` is way more convenient
for me. Your mileage may of course vary.

## Syntax Highlighting

VSC is just better in pretty much every language. That's it.

## TL;DR

I would recommend VSC to everyone looking to become a programmer or people who
are in the market for something. The Neovim learning curve is scary and its
plugion ecosystem sprawling and not as easy to navigate as VSC.

In the long run however nothing beats the sheer speed of Neovim. And I'm pretty
positive that that won't change in the foreseeable future. Language server
plugins are probably going to further close the gap in areas where Neovim is
lacking.

Linting sucks, everywhere.
