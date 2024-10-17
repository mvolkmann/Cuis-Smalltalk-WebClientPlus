# WebClientPlus

This is a package for Cuis Smalltalk that simplifies development of HTTP endpoints
and web applications. It defines classes that extend classes from the WebClient package.
Perhaps the improvments this package makes hould be merged into the WebClient package.

Here is a taste of what it's like to implement a web application using this package.
This uses a bit of htmx which is a client-side JavaScript library
that simplifies web development.
I wrote a book about htmx recently.
See [Server-Diven Web Apps with htmx](https://pragprog.com/titles/mvhtmx/server-driven-web-apps-with-htmx/).

Here's a class for a custom web server:

```smalltalk
WebServerPlus subclass: #HtmxServer
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'WebClientPlus'
```

Static files (only `demo.css` in this example) are served from the `public` directory
which is a subdirectory of the `Cuis-Smalltalk-Dev-UserFiles` directory.

Handlers for routes such as `GET /version`
can be specified with a block or a method selector.

The route for `GET /version` uses a block, which is ideal when the implementation is small.
The block returns text which is the version of Cuis that is running.

The route for `GET /` uses a method selector (`#index:`).
That method returns HTML which is created by the `WebContext` class method `html:`.
That takes an array whose first item is an HTML element name.
The remaining array items can be `Association` objects to specify HTML attributes
or other objects to specify HTML element contents.

The `initialize` method below configures the directory from which static files are served,
and it configures the supported routes.

```smalltalk
initialize
    super initialize.
    self staticFilePath: 'public'.    
    
    self method: #GET path: '/version' handler: [ :context |
        context text: (SystemVersion current versionString)
    ].
    
    self method: #GET path: '/' handler: #index:.
```

The `#index:` method below is used by the `GET /` route.

```smalltalk
index: aWebContext
    | html |
        
    html := WebContext html: {
        #html. #lang->#en.
            {#head.
                {#title. 'My htmx Demo'}.
                {#link. #rel->#stylesheet. #href->'demo.css'}.
                {#script. #src->'https://unpkg.com/htmx.org@2.0.3'}
            }.
            {#body.
                {#h1. 'My htmx Demo'}.
                {#button. 'hx-get'->'/version'. 'hx-target'->'#version'. 'Get Version'}.
                {#div. #id->'version'}
            }
    }.
        
    aWebContext html: html.
```

After cloning this repository, copy the `public` directory
to your `Cuis-Smalltalk-Dev-UserFiles` directory.
Then evaluate the following expressions in a Workspace
to start the web server defined above.

```smalltalk
Feature require: 'WebClientPlus'.
server := HtmxServer new.
server listenOn: 3000.
```

Then browse localhost:3000.
When the "Get Version" button is clicked, a request is sent to the server
to get the version of Cuis Smalltalk that is running.
The text that is returned is placed inside the element with the id "version".

![Screenshot](https://mvolkmann.github.io/blog/assets/Cuis-Smalltalk-WebClientPlus-demo.png)

For a non-trival example, see the `DogWebServer` class which implements endpoints
for all the CRUD operations for managing a collection of dog descriptions.
