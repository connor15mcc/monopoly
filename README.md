# c3110-final
Repo for the 3110 final project

Group: Sunny Chavan, Corban Chiu, Connor McCarthy

## Monopoly Proposal
### Alpha (board but not really a game)
**S:** Board definition (factored out into JSON)
**G:** GUI that displays the above
**E:** Dice rolls, money, movement

### Beta (game with "playability")
**S:** Buying properties (auctions), collecting rent, mortgaging, jail
**G:** Community Chest and Chance (factored into JSON), build houses and hotels,
 free parking
**E:** Multiplayer playable

### Release (real game)
**S:** Multi-person playable, trading properties
**G:** Game menu, win condition
**E:** Bot with different difficulty levels

## Internal Use of Code
- Upon running `make test`, a file `/_coverage/index.html` is created. Opening
this file will show the disect code coverage of the test file.

## Installation Instructions
Mac OS: sudo port install pkgconfig

Windows:

1) apt install pkg-config (dependency for graphics installation)
2) Install Xming and have it running in background

Graphics Package: opam install graphics

## Special Commands
export DISPLAY="$(grep nameserver /etc/resolv.conf | sed 's/nameserver //'):0"

## For Sunny
1) Launch Xming THROUGH xLaunch
  - on the third window check "no Access Control"

2) run Special Command above

###
To make a new branch from VSCode go to branches -> "create new branch from..." -> type the name you want -> select the branch you want to reference (if from the main, "origin/master")

## Ideas

1) Gui
  - add a hover feature that prints info to the right of board that displays square info

# For Demo
- This is a lot more difficult than we thought- will likely not get to build an AI
- Implementing actions of cards, selling certain properties.
