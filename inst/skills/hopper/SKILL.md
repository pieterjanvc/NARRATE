---
name: text-hopper
description: Method to extract words from text using the hopper method
---

# Intro

The hopper method is a way to extract words from piece of text using a few
simple, custom rules.

# Algorithm

1. The first word to return is the first verb in the text
2. The next word is the object that goes with the verb. If there is none, return
   the subject. If that is not present either, continue to step 3
3. Find the next verb in the text, then go back to step 2. If there are no more
   verbs, end.

# Result

The result of a text hop is a structured string formatted like this

```
verb(object);verb[subject];verb;verb[subject]
```

Example

```
ate(apple);tired[James];worry;yell[sara]
```
