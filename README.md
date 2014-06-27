# ember-mode #

Ember-mode speeds up navigation in EmberJS projects.

    NOTE: ember-mode is a work in progress.  Although we use it internally, it will 
    be extended in the future and it isn't fully functional yet.
    
    That being said, it will not destroy your code and it's nice to use.

## Usage ##

Ember-mode helps in jumping between related files in an emberjs project.  It assumes an ember-cli-like folder-structure and supports both coffeescript and javascript extensions.

### example usage ###

Let's assume you have the following files in your emberjs folder structure:


    - app
      router.coffee
        - models
            - friend.coffee
            - plugin.coffee
        - controllers
            - plugins.coffee
        - routes
            - friends.coffee
            - plugins.coffee
            - friends
                - show.coffee

Say you are visiting the `models/friend.coffee` file with `ember-mode` enabled.  Pressing `C-c c r` will move to `routes/friends.coffee`.

If, from there on, you enter `C-c c c` ember-mode will try to open `controllers/friends.coffee`.  As that doesn't exist, it'll try all of the following:

    - controllers/friends.js
    - controllers/friend.coffee
    - controllers/friend.js

As none of these exist, it asks to create `controllers/friends.coffee` instead.

So that's basically it, quickly and easily navigate between files in an emberjs project.


### published bindings ###

    C-c c f c       ember-open-controller
    C-c c f m       ember-open-model
    C-c c f o       ember-open-router
    C-c c f p       ember-open-component
    C-c c f r       ember-open-route
    C-c c f s       ember-open-javascript
    C-c c f t       ember-open-template
    C-c c f v       ember-open-view


## Installation ##

- clone this folder in your emacs.d folder:

    `
    mkdir -p ~/emacs.d/
    cd ~/emacs.d
    git clone https://github.com/madnificent/ember-mode.git
    `

- add the folder to your load path in your `~/.emacs` file if it isn't there already and require ember-mode.
  
    `
    (add-to-list 'load-path "~/emacs.d/ember-mode/")
    (require 'ember-mode)
    `
    
- reload the settings, go to an ember project, enable ember-mode and try out your new keybindings!

   `
   M-x ember-mode
   `


    

    
