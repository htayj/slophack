# TODO

## Level Generation
- [x] Hook up BSP dungeon generation (code exists in world.lisp but only campground is used)
- [x] Add level selection or progression (campground → BSP dungeon)
- [x] Support multiple dungeon levels via z-coordinate
- [ ] Implement Dijkstra pathfinding for smarter tunnel linking between BSP rooms
- [x] Fix wall variant detection to check 8 neighbors (currently only checks 4 cardinal)

## Creatures
- [x] Spawn AcidKop in campground level
- [x] Spawn Ranger, Wook, Egregore variants in levels
- [x] Add AI for non-hippie creatures (Ranger, Wook, AcidKop, Egregores)
- [x] Player pursuit / aggression AI
- [ ] Pathfinding-based creature movement (A* or Dijkstra)

## Items
- [x] Spawn food items in levels (poptart, trailmix, pancake, bacon, soup)
- [x] Spawn drink items in levels (booze, milk, acid)
- [ ] Tools (hammer, nails) — defined in reference but not yet in CL
- [x] Milk subtypes: cow milk vs human milk (hidden from display, shown as "milk")
  - Cow milk: "your bones feel strong"
  - Human milk: "tastes nostalgic"
- [x] Milk freshness: fresh, chunky, or sour (hidden from display)
  - Chunky: "chewy" / "extra thick"
  - Sour: "has a zing to it"
  - Fresh: normal message
- [ ] Bottle/liquid system — bottles and their contents are separate
  - Bottles have a type (water-bottle, gatorade-bottle) and contents (a liquid or empty)
  - Liquids have a type (:water, :gatorade, :booze, :milk, :acid, :piss) and a color
  - Player sees bottle description + liquid color, e.g. "gatorade bottle with blue liquid"
  - Quaffing consumes the liquid, leaves the empty bottle in inventory
  - Bottles can be refilled (e.g. piss into a bottle via `i` in directional prompt)
  - A water bottle could contain something other than water — player only sees color
- [ ] Gatorade — restores hydration + electrolytes
  - Comes in gatorade bottles; colors: yellow, blue, white, red, purple
  - Each color is cosmetic — all gatorade has same stats
- [ ] Liquid colors:
  - Water: clear
  - Gatorade: yellow, blue, white, red, or purple (random)
  - Booze: amber
  - Milk: white (cow) or white (human) — same color, indistinguishable
  - Acid: green
  - Piss: yellow, light yellow, or brown depending on hydration at time of production
    - Well-hydrated: light yellow
    - Normal: yellow
    - Dehydrated: brown

## Actions (nethack-style)
- [x] Eat (e) — select food from inventory, consume it (restore hunger/HP)
- [x] Quaff (q) — select drink from inventory, consume it (various effects)
- [x] Drop (d) — select one item to drop
- [x] Drop multiple (D) — select multiple items to drop
- [x] Pickup (,) — pick up item(s) from ground, select if multiple
- [x] Wait (.) — skip turn
- [x] Remap quit from q to C-q
- [x] Run (shift+HJKLYUBN) — move repeatedly in direction until hitting wall, reaching corridor branch, or spotting a creature
- [x] Look mode (;) — move cursor freely to inspect entities on the map
- [x] Piss (p) — prompted for direction, empties bladder
  - `.` = on self (wet yourself)
  - `>` = on the ground at current tile
  - `<` = up in the air
  - `hjklyubn` = in that direction (hits adjacent tile)
  - `i` = on inventory item
  - Different messages/effects per direction
- [x] General directional action prompt system
  - `.` = on self
  - `>` = on ground at current tile
  - `<` = up in the air
  - `hjklyubn` = adjacent tile in that direction
  - `i` = select item from inventory to apply action on
  - Reusable for piss and future directional actions

## Stats System
- [x] HP / hit points
- [x] Vril / magic points
- [x] Hunger tracking
- [x] Six attributes: Charisma, Strength, Intelligence, Dexterity, Constitution, Wisdom (0-20)
- [x] BUC system (Blessed / Uncursed / Cursed) for items
- [x] Status effects (Confused, with duration tracking)
- [x] Item properties (Fixed, Wet)
- [x] Display stats in status bar
- [x] Hydration (hidden stat) — decrements per turn, never displayed to player
  - Drinking water/milk/booze restores hydration
  - Low hydration alone has no visible warning
- [x] Heat (visible like nethack hunger) — environmental temperature stat
  - Displayed in status bar: "Cool", "Warm", "Hot", "Sweltering"
  - Heat increases hydration drain rate (sweating)
  - Heat + dehydration = heat exhaustion → heat stroke
  - Player only sees status messages ("You feel woozy", "You feel faint")
  - Heat exhaustion: periodic HP loss, "woozy" messages
  - Heat stroke: accelerated HP loss, confusion, eventual death
- [x] BAC (blood alcohol content, hidden stat) — increased by drinking booze
  - Also restores hydration (booze is a liquid)
  - Thresholds with status effects:
    - Tipsy: "You feel loose" (no mechanical penalty)
    - Plastered: "You feel happy" (minor effects)
    - Drunk: "You feel sloppy" (movement inaccuracy — 1-in-3 stumble)
    - Wasted: "You feel confused" (confused status effect)
    - Beyond wasted: increasing chance of entering coma (unconscious, skip turns, vulnerable)
  - BAC decreases over time naturally (-1/turn)
  - Booze does NOT directly cause confusion — only via BAC thresholds
- [x] Bladder fullness (hidden stat) — fills as hydration is consumed
  - Rate of filling proportional to hydration level (dehydrated = barely fills)
  - In heat, bladder fills slower due to fluid lost to sweat
  - When full: status messages ("You need to find a bathroom")
  - Relieved by piss action (p)

## Development
- [x] Live reload: Swank server starts on port 4005 — connect from Emacs (SLIME/SLY) to redefine functions while the game runs

## Inventory
- [x] Inventory screen (i) — view all carried items with letter labels
- [x] Select which item to drop (d prompts with menu)
- [x] Select which item to pick up when multiple on ground (, prompts with menu)
- [x] Item stacking or categorization display
- [ ] Inventory weight/capacity limit

## UI
- [ ] Nethack-style ncurses layout with box-drawing borders around each window (map, status, messages, inventory)
- [x] Message log scrollback
- [x] Help screen showing keybindings
- [x] Fix quit command — C-q now works (raw mode disables XON/XOFF flow control)
- [x] Extended commands (#) — press `#` to open a text prompt, type the full command name
  - e.g. `#quit`, `#eat`, `#drop`, `#look`, `#piss`, etc.
  - Allows access to all commands by name without memorizing keybindings
  - Tab completion with matching suggestions
