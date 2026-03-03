# TODO

## Level Generation
- [ ] Hook up BSP dungeon generation (code exists in world.lisp but only campground is used)
- [ ] Add level selection or progression (campground → BSP dungeon)
- [ ] Support multiple dungeon levels via z-coordinate
- [ ] Implement Dijkstra pathfinding for smarter tunnel linking between BSP rooms
- [ ] Fix wall variant detection to check 8 neighbors (currently only checks 4 cardinal)

## Creatures
- [ ] Spawn AcidKop in campground level
- [ ] Spawn Ranger, Wook, Egregore variants in levels
- [ ] Add AI for non-hippie creatures (Ranger, Wook, AcidKop, Egregores)
- [ ] Player pursuit / aggression AI
- [ ] Pathfinding-based creature movement

## Items
- [ ] Spawn food items in levels (poptart, trailmix, pancake, bacon, soup)
- [ ] Spawn drink items in levels (booze, milk, acid)
- [ ] Tools (hammer, nails) — defined in reference but not yet in CL

## Actions (nethack-style)
- [ ] Eat (e) — select food from inventory, consume it (restore hunger/HP)
- [ ] Quaff (q) — select drink from inventory, consume it (various effects)
- [ ] Drop (d) — select one item to drop
- [ ] Drop multiple (D) — select multiple items to drop
- [ ] Pickup (,) — pick up item(s) from ground
- [ ] Wait (.) — skip turn
- [ ] Remap quit from q to C-q
- [ ] Run (shift+HJKLYUBN) — move repeatedly in direction until hitting wall, reaching corridor branch, or spotting a creature

## Stats System
- [ ] HP / hit points
- [ ] Vril / magic points
- [ ] Hunger tracking
- [ ] Six attributes: Charisma, Strength, Intelligence, Dexterity, Constitution, Wisdom (0-20)
- [ ] BUC system (Blessed / Uncursed / Cursed) for items
- [ ] Status effects (Confused, with duration tracking)
- [ ] Item properties (Fixed, Wet)
- [ ] Display stats in status bar

## Development
- [ ] Live reload: reload changed .lisp files into the running game without restarting (e.g. ASDF reload or Swank/Slime integration)

## Inventory
- [ ] Inventory screen (view all carried items)
- [ ] Select which item to drop (instead of always dropping first item)
- [ ] Select which item to pick up when multiple on ground
- [ ] Item stacking or categorization display
- [ ] Inventory weight/capacity limit

## UI
- [ ] Nethack-style ncurses layout with box-drawing borders around each window (map, status, messages, inventory)
- [ ] Message log scrollback
- [ ] Help screen showing keybindings
