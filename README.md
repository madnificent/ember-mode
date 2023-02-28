# ember-mode #

Ember-mode speeds up navigation in EmberJS projects.

## Usage ##

Ember-mode helps in jumping and generating javascript files in an emberjs project.  It assumes an ember-cli-like folder-structure and supports both TypeScript and JavaScript extensions.  Experimental support for POD structures has recently been added.

### example usage ###

`ember-mode` can be used to either generate, destroy, or navigate between files.

#### example setting ####

Let's assume you have the following files in your emberjs folder structure:


    - app
      router.js
        - models
            - friend.js
            - plugin.js
        - controllers
            - plugins.js
        - routes
            - friends.js
            - plugins.js
            - friends
                - show.js

#### navigation ####

Say you are visiting the `models/friend.js` file with `ember-mode` enabled.  Pressing `C-c . f r` will move to `routes/friends.js`.

If, from there on, you enter `C-c . f c` ember-mode will try to open `controllers/friends.js`.  As that doesn't exist, it'll try all of the following:

    - controllers/friends.js
    - controllers/friend.js

As none of these exist, it lists all controllers and lets you select one of them.


#### manual generation ####

Say we are visiting `routes/plugins.js` and would like to generate a `Peer` model.  Pressing `C-c . g m` will ask for the name of the class to generate (with a default of `plugins`) and the options to be passed to the generator.  The default is hinted based on the current file.

Similarly, if we are visiting `routes/plugins.js` and want to generate a controller, pressing `C-c . g c` will ask for the name of the controller to generate (with a default of `plugins`) and the options to be passed to the generator.  In essence, pressing enter twice will generate `controllers/plugins.js`.


#### automatic generation ####

Say we are visiting `models/friend.js` and want to generate the `friend` route.  Pressing `C-u C-c . g r` will use the ember generators to generate the route and open it (without posing any questions).

Note that pressing `C-c . f r` would have navigated to `routes/friends.js` and wouldn't have found the newly generated route before.


### building, serving, testing ###

`ember build` and `ember test` can be run with `C-c . r b` and `C-c . r t`
respectively, and `C-c . r s` will either run `ember serve`, or bring up
the serve buffer if already running. These commands use compilation
mode to link errors to files with line and column numbers. If the
buffer that `ember serve` runs in is buried, it will also notify of
build status via minibuffer messages.


### working with PODs ###

Ember-mode is shipped with basic support for the POD structure.  Navigation support and generators operate the same, but keep the POD structure in mind.

Ember-mode will pick up `"usePods": true` when set in `.ember-cli`.  If this is set, navigation occurs assuming a POD structure.  See the installation section for more information on overriding the detection of a POD enabled project.

### bonus features ###

ember-mode also supports managing bare-bones imports.  If you need to import computed from the ember package, you can just write `computed` and execute the `C-c . i e` to import computed from the correct package.  This works for other imports too.  If you need to import decorators, many can be accessed through `C-c . i d` when on the symbol.

If you have code which uses the old style `Ember.computed`.  It can help you upgrade an import statement from it's `Ember.something` form to its new form.  Move your cursor on `Ember.computed` and execute `C-c . i u`.


### published bindings ###

    C-c . f c       ember-open-controller
    C-c . f m       ember-open-model
    C-c . f o       ember-open-router
    C-c . f p       ember-open-component
    C-c . f r       ember-open-route
    C-c . f j       ember-open-javascript
    C-c . f t       ember-open-template
    C-c . f v       ember-open-view
    C-c . f x       ember-open-mixin
    C-c . f i       ember-open-initializer
    C-c . f u       ember-open-util
    C-c . f s       ember-open-service
    C-c . f a       ember-toggle-addon

    C-c . g c       ember-generate-controller
    C-c . g m       ember-generate-model
    C-c . g o       ember-generate-router
    C-c . g p       ember-generate-component
    C-c . g r       ember-generate-route
    C-c . g t       ember-generate-template
    C-c . g v       ember-generate-view
    C-c . g x       ember-generate-mixin
    C-c . g i       ember-generate-initializer
    C-c . g u       ember-generate-util
    C-c . g s       ember-generate-service
    C-c . g g       ember-generate

    C-c . d c       ember-destroy-controller
    C-c . d m       ember-destroy-model
    C-c . d o       ember-destroy-router
    C-c . d p       ember-destroy-component
    C-c . d r       ember-destroy-route
    C-c . d t       ember-destroy-template
    C-c . d v       ember-destroy-view
    C-c . d x       ember-destroy-mixin
    C-c . d i       ember-destroy-initializer
    C-c . d u       ember-destroy-util
    C-c . d s       ember-destroy-service
    C-c . d g       ember-destroy

    C-c . r b       ember-build
    C-c . r s       ember-serve-or-display
    C-c . r t       ember-test
        
    C-c . i u       ember-import-upgrade-import-statement-at-point
    C-c . i e       ember-import-from-ember-at-point
    C-c . i d       ember-import-decorator-at-point

If you wish to use a different prefix other than `C-c .`, you may
customize the variable `ember-keymap-prefix` with `M-x
customize-variable`.


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

- to enable ember-mode in specific projects, create a `.dir-locals.el`
  file in your ember project with the following:

        ((nil . ((mode . ember))))
  
  In order to override pod structure detection, you can use the ember-use-pods variable:
  
        ((nil . ((mode . ember)
                 (ember-use-pods . t))))

- alternatively, to enable ember-mode for all javascript and handlebar
  files, assuming you use javascript-mode and web-mode, you would put
  this in your init file:

        (add-hook 'js-mode-hook (lambda () (ember-mode t)))
        (add-hook 'web-mode-hook (lambda () (ember-mode t)))
