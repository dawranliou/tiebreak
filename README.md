# Tiebreak

A tennis game in Common Lisp powered by [raylib][1] through [cffi][3].

## Install

1. Install [raylib][1]
2. Install [cffi][3]
1. Clone this repo
1. Load `:tiebreak`
1. Run `tiebreak:main`

## Build

```
make build
```

## Devlog

### 2022-02-14

Figured out how to project the ball trajectory to 2d perspective and happily
dropped the 3D perspective.  Introduced the first hitting target - static cones.

![Animated tennis ball hitting cone targets](devlog/tiebreak-8-2d-perspective-and-cones.gif)

### 2022-02-10

Drop dependency on [cl-raylib][2] because I want to learn more about how `cffi`
works.  Thank you `cl-raylib` maintainers.

### 2022-02-02

Improved ball hitting physics.

![Animated tennis ball bouncing around the court and the player hit the ball several times](devlog/tiebreak-7-physics.gif)

### 2022-01-22

Rewrote the rendering from 2d to 3d for more realistic physics simulation.

![The court and player on a court in the 3D space](devlog/tiebreak-6-3d.gif)

### 2022-01-20

Start working on the physics.

![A tennis ball dropping from the sky then disappeared below the bottom edge of the window](devlog/tiebreak-5-ball-drop.gif)

### 2022-01-19

![Animatd tennis player sprite with a hitbox indicating the collision zone](devlog/tiebreak-4-hitbox.gif)

### 2022-01-16

![Animated tennis player sprite with rudimentary court](devlog/tiebreak-2-court.gif)

![Animated tennis player swinging the racket on the forehand side and the backhand side](devlog/tiebreak-3-fh-bh-swings.gif)

### 2022-01-15

![Animated tennis player sprite swinging its racket](devlog/tiebreak-1-swing.gif)

## License

MIT

[1]: https://www.raylib.com/
[2]: https://github.com/longlene/cl-raylib
[3]: https://github.com/cffi/cffi
