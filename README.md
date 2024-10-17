# WebClientPlus

This is a package for Cuis Smalltalk that simplifies development of HTTP endpoints
and web applications. It defines classes that extend classes from the WebClient package.
Perhaps this package should be merged into the WebClient package.

Here is a taste of what it's like to implement a web application using this package.
This uses a bit of htmx which is a client-side JavaScript library
that simplifies web development.
I wrote a book about htmx recently.
See https://pragprog.com/titles/mvhtmx/server-driven-web-apps-with-htmx/.

Here's a class for a custom web server:

```smalltalk
WebServerPlus subclass: #HtmxServer
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'WebClientPlus'
```

This simple example has only one instance method, `initialize`.

Static files (only `demo.css` in this example) will be served from the "public" directory
which is a subdirectory of the Cuis-Smalltalk-Dev-UserFiles directory.

Handlers for routes such as "GET /version" can be specified with
a block or a method selector. In this example, I only use blocks for route handlers.

The route for "GET /version" returns text that is the version of Cuis that is running.

The route for "GET /" returns HTML which is created by the WebContext class method "html:".
That takes an array whose first item is an HTML element name.
The remaining array items can be `Association` objects to specify HTML attributes
or other objects to specify HTML element contents.

```smalltalk
initialize		
    super initialize.
    self staticFilePath: 'public'.	
	
    self method: #GET path: '/version' handler: [ :context |
        context text: (SystemVersion current versionString)    ].
	
    self method: #GET path: '/' handler: [ :context |
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
                {#button. 'hx-get'->'/version'. 'hx-target'->'#version'. 'Get Version'}.                    {#div. #id->'version'}
            }
        }.		
        context html: html.
    ].
```

After cloning this repository, evaluate the following lines in a Workspace
to run the web server defined above.

To run this, evaluate the following lines in a workspace:

```smalltalk
Feature require: 'WebClientPlus'.
server := HtmxServer new.
server listenOn: 3000.
```

Then browse localhost:3000.
When the "Get Version" button is clicked, a request is sent to the server
to get the version of Cuis Smalltalk that is running.
The text that is returned is placed inside the element with id "version".
