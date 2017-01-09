This is a modified version of elm-webpack-starter which has been modified to use http://blog.jenkster.com/2016/04/how-i-structure-elm-apps.html sugggestions and 
Html.App.program instead of Html.beginnerProgram which won't get you very far.# elm-webpack-starter

A simple Webpack setup for writing [Elm](http://elm-lang.org/) apps:

* Dev server with live reloading, HMR
* Support for CSS/SCSS (with Autoprefixer), image assets
* Bootstrap 3.3+ (Sass version)
* Bundling and minification for deployment
* Basic app scaffold, using `Html.program`
* A snippet of example code to get you started!


### Install:
Clone this repo into a new project folder, e.g. `my-elm-project`:
```
git clone https://github.com/moarwick/elm-webpack-starter my-elm-project
cd my-elm-project
```

Re-initialize the project folder as your own repo:
```
rm -rf .git         # on Windows: rmdir .git /s /q
git init
git add .
git commit -m 'first commit'
```

Install all dependencies using the handy `reinstall` script:
```
npm run reinstall
```
*This does a clean (re)install of all npm and elm packages, plus a global elm install.*


### Serve locally:
```
npm start
```
* Access app at `http://localhost:8080/`
* Get coding! The entry point file is `src/elm/Main.elm`
* Browser will refresh automatically on any file changes..
* Can now access the page from other devices on the network changing these lines
 in package.json
    -     "start": "webpack-dev-server --hot --inline --content-base src/",
    +     "start": "webpack-dev-server --hot --inline --content-base src/ --host 0.0.0.0",
and webpack.config.js 
 -       'webpack-dev-server/client?http://localhost:8080',
 +       'webpack-dev-server/client?http://0.0.0.0:8080',

 Browser to your ip address followed by :8080 to view on another device.
 i.e. http://192.168.0.10:8080

### Build & bundle for prod:
```
npm run build
```

* Files are saved into the `/dist` folder
* To check it, open `dist/index.html`


### Changelog

**Ver 0.8.2**
* Webpack config improvements (PR by [Lesuk](https://github.com/moarwick/elm-webpack-starter/pull/39))

**Ver 0.8.0**
* Update to Elm 0.18, use `debug=true` on webpack loader (PR by [douglascorrea](https://github.com/moarwick/elm-webpack-starter/pull/33))
* Add a script for one-step installs
* Update to latest packages

**Ver 0.7.1**
* Fix favicon issues, per [Issue 30](https://github.com/moarwick/elm-webpack-starter/issues/30)

**Ver 0.7.0**
* Modify project structure, per [Issue 26](https://github.com/moarwick/elm-webpack-starter/issues/26)
* Include Bootstrap JS, per [Issue 28](https://github.com/moarwick/elm-webpack-starter/issues/28)
* More helpful install steps in README, per [Issue 29](https://github.com/moarwick/elm-webpack-starter/issues/29)
* Update to latest packages

**Ver 0.6.4**
* Changed from  Html.beginnerProgram { model = model, view = view, update = update } to main =
    Html.App.program { init = State.init, update = State.update, subscriptions = State.subscriptions, view = View.root} to be able to do more than just beginner exercices 
* Refactored the code to use the same structure as Kris Jenkins http://blog.jenkster.com/2016/04/how-i-structure-elm-apps.html
* Created a Feature folder to copy and change name to create new features easily.
* Made the Hello Model into a record so that it is part why to a real aplication
* Renamed "main" to "starter"
* Moved Jumbotron and Hello to their own feature folders and set them up
* Access to dev server from other devices via your ip address followed by :8080


**Ver 0.6.2**
* Use `copy-webpack-plugin` instead of `cp` to copy files (Windows compatible)

**Ver 0.6.0**
* `elm-hot-loader` is back (no Elm code changes required!)
* Switch to [bootstrap-sass](https://www.npmjs.com/package/bootstrap-sass) to demo CSS

**Ver 0.5.0**
* Update to Elm 0.17.0 (and other latest modules)
* Upgrade starter code per [upgrade-docs](https://github.com/elm-lang/elm-platform/blob/master/upgrade-docs/0.17.md)
* Remove `elm-hot-loader` (for now)

**Ver 0.4.0**
* Add [elm-hot-loader](https://github.com/fluxxu/elm-hot-loader) for HMR support (PR by [fluxxu](https://github.com/fluxxu))

**Ver 0.3.0**
* Use `html-webpack-plugin` to generate `index.html`
* Apply hash filenames for bundled JS and CSS (prevents caching)
* Image and favicon assets copied to `dist/`
