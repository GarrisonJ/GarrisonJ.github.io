---
layout:   post
title:    Coding music
summary:  Practice, practice
date:     2015-04-21 23:46:06
---

{% highlight haskell %}
d3 $ jux ((|+| speed "0.5") . rev) $ sound "[coeur:4 [coeur:4 coeur:5]]"
|+| speed "0.5 0.75"
|+| pan "1 0 1 0"

d1 $ slow 2 $ sound "808bd:6"
|+| gain "0.8"
|+| vowel "a o e"

d3 $ slow 1 $ sound "808bd:7"
|+| gain "0.8"
|+| vowel "a o e"

d4 $ slow 1 $ sound "[hh:5 sn:1]"
|+| gain "0.5"

d5 $ slow 01 $ sound "cp"
|+| gain "0.75"
{% endhighlight %}

<iframe width="100%" height="300" scrolling="no" frameborder="no" src="https://w.soundcloud.com/player/?url=https%3A//api.soundcloud.com/tracks/160755758&amp;auto_play=false&amp;hide_related=false&amp;show_comments=true&amp;show_user=true&amp;show_reposts=false&amp;visual=true"></iframe>

{% highlight haskell %}
d2 $ sound "drum drum*2 drum drum"

d1 $ sound (every 4 (density 2) "bd:1 bd bd:2 bd:5")
|+| pan (slow 16 sinewave1)

d3 $ every 2 (0.25 <~) 
   $ degradeBy 0.5 
   $ every 2 (rev) 
   $ sound (every 4 (density 2) "bass:1 bass:2")

d5 $ every 2 (0.25 <~) $ sound (every 4 (density 2) "bass:1 bass:2")
|+| density 3 (vowel "a e o")

d4 $ sound (every 4 (density 4 . slow 2) "tech:4*4")
|+| shape ((/ 2) <$> sinewave1)
{% endhighlight %}

<iframe width="100%" height="300" scrolling="no" frameborder="no" src="https://w.soundcloud.com/player/?url=https%3A//api.soundcloud.com/tracks/159288213&amp;auto_play=false&amp;hide_related=false&amp;show_comments=true&amp;show_user=true&amp;show_reposts=false&amp;visual=true"></iframe>

### Made with ___[tidal](http://tidal.lurk.org/)___
