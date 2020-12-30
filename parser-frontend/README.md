# parser-frontend

## Building the app

### For development
To build the application run `elm make src/Main.elm --output app.min.js`.
<br>
The resulting `app.min.js` is obviously not minified, but naming it like that saves having either another `index.html` for production or manually changing the name for every production build.

To then run it open the `index.html` in your browser or serve it via some http server like the node module `http-server-spa` (`http-server-spa . index.html 80`).

### For production
To build the application I wrote a `build_for_production.cmd` script.
It first runs `elm make src/Main.elm --optimize --output app.min.js`, then uglifies and minifies the resulting javascript file and in the and zips it together with the `index.html` and all `assets`.
<br>
For more see the [elm docs](https://guide.elm-lang.org/optimization/asset_size.html).

Now you can host it via a webserver of your choice. In my case it is hosted on an
nginx server. I copied the zip file into nginx's `html` folder, unzipped it and copied the contents into the `html` folder. You could also unzip it somewhere else and just copy the contents.
<br>
I then used the following nginx config to run it.
```
server {
    listen       80;
    server_name  localhost;
    index index.html;
    root html;

    location / {
      try_files $uri $uri/ /index.html;
    }

    # redirect server error pages to the static page /50x.html
    #
    error_page   500 502 503 504  /50x.html;
    location = /50x.html {
      root   html;
    }
}
```
