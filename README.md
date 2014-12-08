# ember-mode #

Ember-mode speeds up navigation in EmberJS projects.

    NOTE: ember-mode is a work in progress.  Although we use it internally, it will 
    be extended in the future and it isn't fully functional yet.
    
    That being said, it will not destroy your code and it's nice to use.

## Usage ##

Ember-mode helps in jumping and generating javascript files in an emberjs project.  It assumes an ember-cli-like folder-structure and supports both coffeescript and javascript extensions.

### example usage ###

`ember-mode` can be used to either generate, or navigate between files.

#### example setting ####

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

#### navigation ####

Say you are visiting the `models/friend.coffee` file with `ember-mode` enabled.  Pressing `C-c c f r` will move to `routes/friends.coffee`.

If, from there on, you enter `C-c c f c` ember-mode will try to open `controllers/friends.coffee`.  As that doesn't exist, it'll try all of the following:

    - controllers/friends.js
    - controllers/friend.coffee
    - controllers/friend.js

As none of these exist, it lists all controllers and lets you select one of them.


#### manual generation ####

Say we are visiting `routes/plugins.coffee` and would like to generate a `Peer` model.  Pressing `C-c c g m` will ask for the name of the class to generate (with a default of `plugins`) and the options to be passed to the generator.  The default is hinted based on the current file.

Similarly, if we are visiting `routes/plugins.coffee` and want to generate a controller, pressing `C-c c g c` will ask for the name of the controller to generate (with a default of `plugins`) and the options to be passed to the generator.  In essence, pressing enter twice will generate `controllers/plugins.coffee`.


#### automatic generation ####

Say we are visiting `models/friend.coffee` and want to generate the `friend` route.  Pressing `C-u C-c c g r` will use the ember generators to generate the route and open it (without posing any questions).

Note that pressing `C-c c f r` would have navigated to `routes/friends.coffee` and wouldn't have found the newly generated route before.


*PRO TIP:* If you are using coffeescript, enable `ember-cli-coffeescript` to generate coffeescript instead of javascript files.  If you don't, javascript files will be generated.

    cd path/to/your/ember-cli-project
    npm install ember-cli-coffeescript --save-dev
    

### published bindings ###

    C-c c f c       ember-open-controller
    C-c c f m       ember-open-model
    C-c c f o       ember-open-router
    C-c c f p       ember-open-component
    C-c c f r       ember-open-route
    C-c c f j       ember-open-javascript
    C-c c f t       ember-open-template
    C-c c f v       ember-open-view
    C-c c f x       ember-open-mixin
    C-c c f i       ember-open-initializer
    C-c c f u       ember-open-util

    C-c c g c       ember-generate-controller
    C-c c g m       ember-generate-model
    C-c c g o       ember-generate-router
    C-c c g p       ember-generate-component
    C-c c g r       ember-generate-route
    C-c c g j       ember-generate-javascript
    C-c c g t       ember-generate-template
    C-c c g v       ember-generate-view
    C-c c g x       ember-generate-mixin
    C-c c g i       ember-generate-initializer
    C-c c g u       ember-generate-util
    C-c c g g       ember-generate


## Installation ##

- clone this folder in your emacs.d folder:

        mkdir -p ~/.emacs.d/
        cd ~/.emacs.d
        git clone https://github.com/madnificent/ember-mode.git


- add the folder to your load path in your `~/.emacs` file if it isn't there already and require ember-mode.

        (add-to-list 'load-path "~/.emacs.d/ember-mode/")
        (require 'ember-mode)

    
- reload the settings, go to an ember project, enable ember-mode and try out your new keybindings!

        M-x ember-mode

