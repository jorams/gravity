# Gravity

This is a little toy probject to create a gravity-like simulation in Common Lisp. It only looks a little bit like gravity, but isn't remotely accurate.

## Dependencies

- [LISPBUILDER-SDL](https://code.google.com/p/lispbuilder/wiki/LispbuilderSDL) (MIT)
- [Bordeaux Threads](http://common-lisp.net/project/bordeaux-threads/) (MIT)
- [Alexandria](http://common-lisp.net/project/alexandria/) (Public Domain)

## Quickstart

To get this thing started put it somewhere [Quicklisp][] can find it and execute the following in the REPL:

```lisp
(ql:quickload :gravity)
(gravity:start)
```

You can add new planets using the left mouse button. Drag to set the new planet's movement speed. You can set the size (and thereby the mass) of a planet by dragging the right mouse button. A preview of the new size and movement will be shown. Regardless of the movement speed set, the middle mouse button creates a planet with a movement speed of 0.

The following keybinds are available:

| Key               | Result                                                |
|-------------------|-------------------------------------------------------|
| `ESC`             | Pause the game.                                       |
| `q` (when paused) | Quit                                                  |
| `n`               | Skip one frame ahead (Only really useful when paused) |
| `d`               | Toggle clearing the screen every frame                |
| `p`               | Toggle drawing the planet preview                     |
| `r`               | Reset the new planet movement speed to 0              |
| `c`               | Remove all planets                                    |
| `b`               | Toggle bouncing at the screen's edge                  |
| `a`               | Toggle planet auto-creation. (On every mouse move)    |

[Quicklisp]: http://www.quicklisp.org/

## License

    Copyright (c) 2014 Joram Schrijver

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.
