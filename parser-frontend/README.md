# parser-frontend

## Building the app

### For development
To build the application run `elm make src/Main.elm --output app.js`.
<br>
To then run it open the `index.html` in your browser or serve it via some http server like the node module `http-server-spa` (`http-server-spa . index.html 80`).

### For production
To build the application run `elm make src/Main.elm --optimize --output app.js`.
<br>
Now you can host it via a webserver of your choice. In my case it is hosted on an
nginx server. I copied all the necessary files (`assets/`, `app.js`, `index.html`)
into nginx's `html` folder and used the following config to run it.
```
server {
    listen       80;
    server_name  localhost;

    location / {
      root   html;
      index  index.html index.htm;
    }

    # redirect server error pages to the static page /50x.html
    #
    error_page   500 502 503 504  /50x.html;
    location = /50x.html {
       root   html;
    }
  }
```
