# YAP

## Unified language for authoring dialogs for games
### Very early - Do not use it for production

[![Youtube Video](https://img.youtube.com/vi/e515wvgOHQk/0.jpg)](https://www.youtube.com/watch?v=e515wvgOHQk)

This is a simpler alternative to YarnSpinner.
Designed to not dictate much of your workflow, so it can be easilly integrated, and to allow for writers to work comfortably, using tools they are fammiliar with, and not putting the burden of writing complex game logic and handling states in the scripts. Writers would write dialogs, and programmers can hook into it to influence game state.
Scripts would be the single source of truth for anything related to dialogs, no data duplication.

### To do
 - Branching
   - Based on player choices
   - Based on game state conditions, to be resolved at runtime
 - Arbitrary tags that can be resolved at runtime (like amount of coins held by the player)
 - Arbitrary line metada that can be resolved at runtime (like a emote, or mood, or facial expression)
 - Events
 - Code generation to expose script data, like actor names and tags
 - Automatic compilation of scripts when changed
 - Built in localization support
 - Csv exporter
 - Node based vizualization - Read only
 - Sublime Text extension
 - Visual Code extension
 - Google docs extension 

### How to build
 - Download the Odin compiler using this [link](https://github.com/odin-lang/Odin/releases).
 - Make sure its added to your enviroment variables
 - Run build.bat
