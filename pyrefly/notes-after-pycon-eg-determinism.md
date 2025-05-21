Narrow things to do on Pyrefly per se:
- Determinism, obviously
  - This is very much the top priority once I think I can handle it
- Clean up and document some narrowing logic
  - It is not remotely okay to have two versions of the identifier chain logic that duplicate almost 100 lines of fiddly code!
  - Also, it's not okay for these functions to just say what they do, they need to explain why. Diff summaries aren't
    good enough... the reason we need both is so we can invalidate *all* indexes on a narrow (we should invalidate gets too,
    when we have them)
- Add Get narrowing


Things to do bigger picture:
- Post a call for typing meetup topics
- Figure out a plan for my enablement work
  - Projects I'd really like to drive:
    - Sphinx
    - Helion
  - My overall focus (but other folks are doing the 'driving')
    - Pytorch ... do reach out to the contact!
    - Instagram
- Figure out a cadence for going through github issues and PRs;
  probably EOD is best b/c Danny will grab anything that comes in after work hours so we'll split the work that way.



For narrowing, my thoughts about how I could do things:
- The most principled approach would be to use callbacks, but this seems
  pretty nasty in terms of the impact on code structure. I could potentially
  try it in a few places to see how bad it is.
- I think it would work pretty well - but would not ensure people do things in
  the correct order - to use a stack where we call a function to turn a Key
  into an Idx<Key>, and then when we insert we are required to pass the Idx<Key>
  which gets checked in order to ensure stack discipline (plus we also check
  that the stack is empty at the end)
  - A disadvantage in doing this is that you could still reproduce the existing
    pattern of just making the Idx at the last second, which would be buggy
  - But actually, this might be okay. My thoughts here are that:
    - Many kinds of bindings are fine to insert in one shot anyway (especially
      usage, but lots of others too - maybe most!)
    - It's actually pretty clear-cut which ones *aren't* okay to just insert:
      the ones that use `ensure_.*` are exactly the ones relevant to bindings
      determinism... with a caveat I'll get to
    - So one way to get some decent guarantees would be to pass a placeholder
      `Idx<Key>` (likely wrapped in some marker type, so that people don't
      get clowny trying to satisfy the types without understanding the purpose)
      to `ensure_*`, to prove that you are in fact following the stack
      discipline
    - ... the caveat is that I'm not certain that, 100% of the time, we want
      the actively-in-construction binding to be the one that "owns" a
      usage. I *think* we probably do most of the time, but there might be
      weird edge cases. Maybe it's better to assume the simple thing and
      wait for edge cases to disprove, at worst we get some extra Anys
    - but here's the exciting bit: if we make the change above, then I think
      we may be able to actually get away without the usage accumulator, which
      is an annoying part of the nondeterminism design - if we have the
      relevant Idx<Key> when we call `ensure_.*`, then we should already
      have enough information to insert it into any relevant
      def-without-first-usage bindings that need updates!
      - So one of the trickier bits of the design disappears (admittedly at the
        cost of a big refactor, but I think the refactor is a good idea
        for BE reasons anyway)




Next issue: figure out what's going on in this example
```
from typing import *

class A:
    kind: str

def test(y: A | None) -> None:
    if not y or y.kind:
        reveal_type(y)
```
(there might be multiple bugs!)



```rust
testcase!(
    stress_test_join_more,
    r#"
from typing import *

class A:
    kind: str

def test(y: A | None) -> None:
    if y:
        reveal_type(y)
        if y.kind:
            reveal_type(y)
        else:
            reveal_type(y)
        reveal_type(y)
    else:
        reveal_type(y)
    reveal_type(y)
"#,
);
````