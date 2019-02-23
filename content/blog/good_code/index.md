---
title: How not to Write Crap Code
date: "2019-01-08"
---

Yesterday I submitted what was probably my worst code over the past 12 months. Not because I used the wrong data structure or called something on `undefined`, but simply because I was the worst possible version of myself. That must not happen again. I'm human, I can learn from my mistakes... right?

# Don't Commit After Midnight

In _Why We Sleep_ Matthew Walker mentions a study that compared two groups: One group didn't sleep for a single night, the other had reached the alcohol limit for driving (page 156-157). In a subsequent concentration test, both groups performed about the same. The author of the book then compares this to getting up at 7am, and eventually driving a car at 2am, after having been awake for 19 hours. In other words, at the end of a normal day your concentration could very well be as bad as if you were drunk enough to not being allowed to drive.

The code I mentioned in the beginnig was committed at around 2am in the morning. It's no surprise that my ability to focus was seriously impaired and in the context of studies like the above it's pretty crazy to think you can write code in the middle of the night (unless you get up in the afternoon). _(Isn't there a *How I Met Your Mother* quote about nothing good every happening after 2am or something?)_

Our society values the worker who's still in the office long after everyone else has left. That person may not be capable of doing anything productive anymore, and might even do more harm than good, but no one cares about that I guess. Know when you're reached your limit for the day and then focus on recovery. Just like athletes don't work out 20h a day, you shouldn't code that much either. It's not going to work. You'll write terrible code and you'll regret it. Just go to bed, and come back refreshed.

# Take a Walk

I had some of my best ideas when I miss the tram and have to walk home, which takes about 15min. It is during those minutes, without a computer, without notifications, music, reddit or hackernews, that I can let my mind wander. The emptiness is often filled with random crap but sometimes it also unleashes creative problem solving skills that are surprisingly elusive in the office.

One of the main benefits of leaving your PC for a while is that it lets you take a step back, zoom out and really see the big picture. Sometimes I start trying to solve a problem in a certain way and then I double down and brute force my way to a solution even though a totally different approach might have made way more sense had I only considered it. You won't reach those conclusion if you don't let your mind roam freely for a while.

# No Multitasking

Many of my coworkers wear headphones all day long. That makes perfect sense since many of them sit in open offices. But isolating yourself from your surroundings only to listen to songs with lyrics won't help. If you need some sort of background noise to focus then I'd suggest giving something like Brain.fm a try. Music that can enhance producitivy has to follow certain patterns (range of beats per minute, no lyrics, somewhat repetitive and predictable, etc.) and I'm pretty sure that most of what people listen to doesn't meet those criteria. Even if you're convinced that it helps, do you really want to treat music as mere background noise? Best case scenario is that you notice and follow the songs at least during the catchiest part of the chorus but apart from that you won't really remember much. Music is too beautiful to be one blip in the general office cacophony. So do yourself, and your music a favor by giving some whitenoise or procedural music generator a try. Also it goes without saying that watching Netflix while working is just ridiculous.

# Take Regular Breaks

This was already mentioned in the prior paragraphs but regular, short breaks can do wonders for your mental energy. Avoid bashing your head against a wall at all costs and keep up your creative energy so you can focus on solving problems instead of just implementing whatever the ticket says.

# Skip Stack Overflow

Stack Overflow (SO) has probably done more harm than good to my code so far. Most of the accepted answers that I come across are rather short and there's only so much one can explain in one or two paragraphs. So you most likely end up copy pasting a solution you don't fully understand and that's both dangerous and just not very efficient. Next time you need to solve a related problem you'll need to find another SO post to copy verbatim, since you didn't understand what you did in the first place.

Being able to read documentation and piece things together on your own is a valuable skill, so practice that. Like all rules this there are of course exceptions. But in general I've learned much more from looking at the hackage documentation, the node API docs or the rust stdlib docs, rather than SO.
