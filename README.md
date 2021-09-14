# Building the project
This project is written primarily in PureScript, so you will need a number of tools to compile it into JavaScript.

These tools can all be installed using npm, the Node package manager.

To build a production version of the application, simply run
```
 npm install
```

followed by

```
 npm run prod_windows
```

or 

```
 npm run prod_linux
```

depending on the operating system.

This will create a folder `dist` containing a number of HTML, CSS, and JavaScript files, which can then be hosted on any web server.
If you want to run the application locally, you can simply open the file `index.html` in this folder to run the application in your browser.

The folder will also contain some files ending in `.map`.
These files are only necessary for debugging, so there is no reason to copy them to the web server when hosting the application online.