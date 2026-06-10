# Get started with jugglr

``` r

library(jugglr)
```

Juggling sequences can be written in a notation called siteswap. A
3-ball cascade — the first sequence most jugglers learn — is written as
“3”, where each ball is thrown the same height and caught in the
opposite hand three beats later. “531”, “423” and “441” are also valid
3-ball juggling patterns, though with balls thrown to different heights,
or caught in the same hand that throws them. Each number in a siteswap
sequence encodes how many beats until that prop needs to be thrown
again. However, not everything that can be written in siteswap is a
valid/juggleable pattern. For the example, in the sequence “432”, the
first two props thrown would need to be caught in the same hand at the
same time.

**jugglr** lets you create, validate, and visualise siteswap patterns in
R.

There are several different types of siteswap, which can be distinguised
through the notations: vanilla, sychronous, multiplex, synchronous
multiplex, and passing, each explained in the [types of
siteswap](https://ellakaye.github.io/jugglr/articles/LINK) section
below.

In **jugglr**, we define a sequence with the function
[`siteswap()`](https://ellakaye.github.io/jugglr/reference/Siteswap.md),
which creates an [S7](https://ellakaye.github.io/jugglr/articles/URL)
object with class `Siteswap` as well as a child class corresponding to
its type, i.e. `vanillaSiteswap`, `synchronousSiteswap`,
`multiplexSiteswap`, `synchronousMultiplexSiteswap`, or
`passingSiteswap`.

For each child class, there is a `print` method defined, showing the
sequence and [whether it is
valid](https://ellakaye.github.io/jugglr/articles/URL). For valid
sequences, we also see the number of props it requires, its period (how
many beats before it repeats), and whether it is symmetrical
(i.e. whether both hands do the same this, just offset in time). For
patterns that aren’t valid, we see why not (either because it does not
satisfy the average theorem, or because it has collisions).

There are also two visualisation methods that work for each class:
[`timeline()`](https://ellakaye.github.io/jugglr/reference/timeline.md)
which shows the beats on which each prop is thrown and caught, and
[`ladder()`](https://ellakaye.github.io/jugglr/reference/ladder.md)
which converys that information, as well as which hands the props are
thrown from and caught it. These visualisations are useful both for
seeing how valid sequences fit together, as well as for diagnosing why
invalid sequences do not work.

## Vanilla siteswap

The simplest form is *vanilla* siteswap: one prop thrown per beat, hands
alternating. Create a pattern with
[`siteswap()`](https://ellakaye.github.io/jugglr/reference/Siteswap.md):

``` r

siteswap("3")
#> ✔ '3' is valid vanilla siteswap
#> ℹ It uses 3 props
#> ℹ It is symmetrical with period 1
```

The print method tells you four things: whether the pattern is valid,
how many props it requires, its period (how many beats before it
repeats), and whether it’s symmetrical — meaning both hands do the same
thing, just offset in time.

Here’s a trickier pattern:

``` r

ss_423 <- siteswap("423")
ss_423
#> ✔ '423' is valid vanilla siteswap
#> ℹ It uses 3 props
#> ℹ It is symmetrical with period 3
```

Still three props, period 3. The 4 is a high self-throw that buys you a
moment, the 2 is a low hold, and the 3 is a standard cascade throw. The
pattern starts to take shape even from the numbers. And for the jugglers
working towards five balls:

``` r

siteswap("5")
#> ✔ '5' is valid vanilla siteswap
#> ℹ It uses 5 props
#> ℹ It is symmetrical with period 1
```

### Invalid patterns

Not every sequence of numbers makes a jugglable pattern. jugglr checks
validity at construction:

``` r

ss_21 <- siteswap("21")
ss_21
#> ✖ '21' is not a valid juggling pattern
#> ℹ The throws don't average to a whole number
#> ℹ Two or more throws land on the same beat (collision)
```

Two problems, both reported separately. The average of 2 and 1 is 1.5,
which would require 1.5 props — impossible. And even setting that aside,
two throws would land on the same beat: a collision. The diagrams in the
[visualising patterns](#visualising-patterns) section below make this
concrete.

This is genuinely useful when you’re inventing patterns. Check the
sequence before picking up the clubs.

## Other notation types

Vanilla siteswap is one prop per beat from alternating hands. jugglr
handles four more notation types for more complex juggling styles.

### Synchronous siteswap

Both hands throw at the same time, written as pairs in parentheses:

``` r

siteswap("(4,4)")
#> ✔ '(4,4)' is valid synchronous siteswap
#> ℹ It uses 4 props
#> ℹ It is symmetrical with period 2
```

An `x` suffix on a throw value marks it as crossing to the other side.
The `*` shorthand indicates the pattern alternates between two states —
jugglr expands it into the full sequence:

``` r

siteswap("(4,2x)*")
#> ✔ '(4,2x)*' is valid synchronous siteswap
#> ℹ Full sequence: (4,2x)(2x,4)
#> ℹ It uses 3 props
#> ℹ It is symmetrical with period 4
```

### Multiplex siteswap

Multiple props thrown from one hand simultaneously, with square brackets
grouping them:

``` r

siteswap("[43]1")
#> ✔ '[43]1' is valid multiplex siteswap
#> ℹ It uses 4 props
#> ℹ It is asymmetrical with period 2
```

### Synchronous multiplex siteswap

Both hands throwing at the same time, with multiplex groups:

``` r

siteswap("(4,[42x])*")
#> ✔ '(4,[42x])*' is valid synchronous multiplex siteswap
#> ℹ Full sequence: (4,[42x])([42x],4)
#> ℹ It uses 5 props
#> ℹ It is symmetrical with period 4
```

### Passing siteswap

Patterns for multiple jugglers. The `<A|B>` notation separates each
juggler’s sequence; `p` on a throw value marks a pass to the other
juggler:

``` r

siteswap("<3p 3|3p 3>")
#> ✔ '<3p 3|3p 3>' is valid passing siteswap
#> ℹ It uses 6 props across 2 jugglers
#> ℹ It is asymmetrical with period 2
```

Six props between two jugglers, each passing on every other throw.
Experienced passers often prefer fractional notation, where a `.5` on a
throw indicates it’s a pass:

``` r

siteswap("<4.5 3 3 | 3 4 3.5>")
#> ✔ '<4.5 3 3 | 3 4 3.5>' is valid passing siteswap
#> ℹ It uses 7 props across 2 jugglers
#> ℹ It is symmetrical with period 3
```

## Visualising patterns

A sequence of numbers only tells you so much about a pattern. jugglr
provides two diagrams, each revealing different aspects of the same
sequence.

Both
[`timeline()`](https://ellakaye.github.io/jugglr/reference/timeline.md)
and [`ladder()`](https://ellakaye.github.io/jugglr/reference/ladder.md)
return ggplot2 objects, so you can customise them further with standard
ggplot2 functions.

### Timeline

[`timeline()`](https://ellakaye.github.io/jugglr/reference/timeline.md)
draws arcs: each arc represents one throw, with height proportional to
the throw value, colour-coded by prop. It’s roughly what the pattern
looks like from the side.

``` r

timeline(ss_423)
```

![Timeline arc diagram for the 423 pattern, showing three arcs per cycle
colour-coded by prop](jugglr_files/figure-html/timeline-valid-1.png)

Follow any single colour to track one prop through the pattern. Each arc
shows when it was thrown, how high, and when it lands.

### Ladder diagram

[`ladder()`](https://ellakaye.github.io/jugglr/reference/ladder.md)
shows the schedule: beats across the axis, with lines connecting throws
to catches. Straight lines are self-throws; crossing lines go to the
other hand.

``` r

ladder(ss_423)
```

![Ladder diagram for the 423 pattern showing throws and catches
connected by lines](jugglr_files/figure-html/ladder-valid-1.png)

Where the timeline shows the shape of the pattern in the air, the ladder
shows the mechanics hand-to-hand.

### Invalid patterns

Both diagrams become most instructive when something goes wrong:

``` r

timeline(ss_21)
```

![Timeline diagram for the invalid 21 pattern, showing a collision where
two arcs land on the same
beat](jugglr_files/figure-html/timeline-invalid-1.png)

``` r

ladder(ss_21)
```

![Ladder diagram for the invalid 21 pattern, showing two lines
converging at the same
beat](jugglr_files/figure-html/ladder-invalid-1.png)

The collision is visible: two arcs land on the same beat, two lines
converge at the same point. The colours show where props appear out of
nowhere or need to be in two places at once. This is a useful debugging
tool when an invented pattern doesn’t feel right — the diagram tells you
exactly where it breaks.

### Passing patterns

Both diagrams extend naturally to passing patterns, with one lane per
juggler:

``` r

ss_pass <- siteswap("<3p 3|3p 3>")
timeline(ss_pass)
```

![Timeline arc diagram for the \<3p 3\|3p 3\> passing pattern, with two
juggler lanes and passes arcing between
them](jugglr_files/figure-html/timeline-passing-1.png)

``` r

ladder(ss_pass)
```

![Ladder diagram for the \<3p 3\|3p 3\> passing pattern, with two
juggler rows and passes shown as diagonal
lines](jugglr_files/figure-html/ladder-passing-1.png)

Passes appear as arcs (or lines) that cross between the juggler lanes.

### A few options

The `n_cycles` argument controls how many repetitions to show — the
default of 3 is usually enough to see the full structure, but increase
it to trace individual props further.
[`ladder()`](https://ellakaye.github.io/jugglr/reference/ladder.md) also
accepts `direction = "vertical"` if you prefer that orientation.

## The raw data

If you want to build your own visualisation or work directly with the
numbers,
[`throw_data()`](https://ellakaye.github.io/jugglr/reference/throw_data.md)
returns the underlying data frame:

``` r

throw_data(ss_423)
#>   beat hand throw catch_beat catch_hand prop
#> 1    1    0     4          5          0    1
#> 2    2    1     2          4          1    2
#> 3    3    0     3          6          1    3
#> 4    4    1     4          8          1    2
#> 5    5    0     2          7          0    1
#> 6    6    1     3          9          0    3
#> 7    7    0     4         11          0    1
#> 8    8    1     2         10          1    2
#> 9    9    0     3         12          1    3
```

One row per throw: when it was thrown, which hand, the throw value, when
and where it lands, and which prop it belongs to. This is the same data
that drives the diagrams.

## Animation

jugglr can also produce animated GIFs of patterns via the
[JugglingLab](https://jugglinglab.org) server. Here’s `423` animated
with three colours:

``` r

animate("423", colors = c("#E69F00", "#56B4E9", "#009E73"))
```

![Animated GIF of the 423 juggling pattern with three coloured
balls](figures/423-animation.gif)

The `animate` vignette covers the full range of options: colour modes,
prop types, speed controls, and how to save animations to disk for use
in documents.
