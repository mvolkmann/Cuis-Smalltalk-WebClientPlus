'From Cuis7.1 [latest update: #6757] on 13 October 2024 at 6:46:03 pm'!
'Description '!
!provides: 'WebClientPlus' 1 18!
!requires: 'WebClient' 1 37 nil!
!requires: 'JSON' 1 26 nil!
!requires: 'Regex-Core' 1 6 nil!
SystemOrganization addCategory: #WebClientPlus!


!classDefinition: #DogWebServerTests category: #WebClientPlus!
TestCase subclass: #DogWebServerTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'WebClientPlus'!
!classDefinition: 'DogWebServerTests class' category: #WebClientPlus!
DogWebServerTests class
	instanceVariableNames: ''!

!classDefinition: #HTMLTests category: #WebClientPlus!
TestCase subclass: #HTMLTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'WebClientPlus'!
!classDefinition: 'HTMLTests class' category: #WebClientPlus!
HTMLTests class
	instanceVariableNames: ''!

!classDefinition: #WebClientPlus category: #WebClientPlus!
WebClient subclass: #WebClientPlus
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'WebClientPlus'!
!classDefinition: 'WebClientPlus class' category: #WebClientPlus!
WebClientPlus class
	instanceVariableNames: ''!

!classDefinition: #WebServerPlus category: #WebClientPlus!
WebServer subclass: #WebServerPlus
	instanceVariableNames: 'action dynamicRoutes methods options staticFilePath staticRoutes'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'WebClientPlus'!
!classDefinition: 'WebServerPlus class' category: #WebClientPlus!
WebServerPlus class
	instanceVariableNames: ''!

!classDefinition: #DogWebServer category: #WebClientPlus!
WebServerPlus subclass: #DogWebServer
	instanceVariableNames: 'dogDict'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'WebClientPlus'!
!classDefinition: 'DogWebServer class' category: #WebClientPlus!
DogWebServer class
	instanceVariableNames: ''!

!classDefinition: #Dog category: #WebClientPlus!
Object subclass: #Dog
	instanceVariableNames: 'breed id name'
	classVariableNames: 'Count LastId'
	poolDictionaries: ''
	category: 'WebClientPlus'!
!classDefinition: 'Dog class' category: #WebClientPlus!
Dog class
	instanceVariableNames: ''!

!classDefinition: #HTML category: #WebClientPlus!
Object subclass: #HTML
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'WebClientPlus'!
!classDefinition: 'HTML class' category: #WebClientPlus!
HTML class
	instanceVariableNames: ''!

!classDefinition: #MimeType category: #WebClientPlus!
Object subclass: #MimeType
	instanceVariableNames: ''
	classVariableNames: 'ExtensionToMimeDict'
	poolDictionaries: ''
	category: 'WebClientPlus'!
!classDefinition: 'MimeType class' category: #WebClientPlus!
MimeType class
	instanceVariableNames: ''!

!classDefinition: #WebContext category: #WebClientPlus!
Object subclass: #WebContext
	instanceVariableNames: 'request route'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'WebClientPlus'!
!classDefinition: 'WebContext class' category: #WebClientPlus!
WebContext class
	instanceVariableNames: ''!

!classDefinition: #WebRoute category: #WebClientPlus!
Object subclass: #WebRoute
	instanceVariableNames: 'handler method path pattern'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'WebClientPlus'!
!classDefinition: 'WebRoute class' category: #WebClientPlus!
WebRoute class
	instanceVariableNames: 'routes'!


!DogWebServerTests methodsFor: 'testing' stamp: 'RMV 10/12/2024 19:49:25'!
setUp
	| dog port url |
	
	url := self baseUrl.
	
	"If the web server is not running, start it."
	port := 3000.
	[
		Socket newTCP connectToHostNamed: 'localhost' port: port.
	] on: ConnectionTimedOut do: [
		'starting web server' print.
		DogWebServer new listenOn: port.	
	].

	"Delete all the dogs."
	WebClientPlus method: #DELETE url: url.

	"Create initial set of dogs"
	dog := Dog name: 'Comet' breed: 'Whippet'.
	WebClientPlus method: #POST url: url content: dog asJsonString.
	dog := Dog name: 'Oscar' breed: 'German Shorthaired Pointer'.
	WebClientPlus method: #POST url: url content: dog asJsonString.! !

!DogWebServerTests methodsFor: 'testing' stamp: 'RMV 10/12/2024 15:40:20'!
testDelete
	| baseUrl dog dogs jsonArray res url |

	"Get any one of the current dogs."
	baseUrl := self baseUrl.
	res := WebClientPlus method: #GET url: baseUrl.
	self assert: [res code = 200] description: 'OK status'.
	jsonArray := Json readFrom: res stream.
	dogs := jsonArray collect: [ :jsonObject | Dog fromJsonObject: jsonObject ].
	dog := dogs first.

	"Delete the dog."
	url := '{1}/{2}' format: {baseUrl. dog id}.
	res := WebClientPlus method: #DELETE url: url.
	self assert: res code equals: 200.
	
	"Try to delete the dog again."
	res := WebClientPlus method: #DELETE url: url.
	self assert: res code equals: 404.
	! !

!DogWebServerTests methodsFor: 'testing' stamp: 'RMV 10/9/2024 18:58:24'!
testGetAll
	| dogs expected jsonArray res url |

	url := self baseUrl.
	res := WebClientPlus method: #GET url: url.	
	self assert: res code equals: 200.
	
	jsonArray := Json readFrom: res stream.
	dogs := jsonArray collect: [ :jsonObject | Dog fromJsonObject: jsonObject ].
	dogs sort: [:a :b | a name < b name].
	expected := {
		Dog name: 'Comet' breed: 'Whippet'.
		Dog name: 'Oscar' breed: 'German Shorthaired Pointer'.
	}.
	self assert: [dogs = expected] description: 'correct response body'.
	! !

!DogWebServerTests methodsFor: 'testing' stamp: 'RMV 10/9/2024 18:58:29'!
testGetById
	| baseUrl dog dogs json jsonArray res url |

	"Get any one of the current dogs."
	baseUrl := self baseUrl.
	res := WebClientPlus method: #GET url: baseUrl.
	self assert: res code equals: 200.
	jsonArray := Json readFrom: res stream.
	dogs := jsonArray collect: [ :jsonObject | Dog fromJsonObject: jsonObject ].
	dog := dogs first.

	"Attempt to get that dog by its id."
	url := '{1}/{2}' format: {baseUrl. dog id}.
	res := WebClientPlus method: #GET url: url.
	self assert: res code equals: 200.
	json := Json readFrom: res stream.
	self assert: (json at: #name) equals: dog name.
	self assert: (json at: #breed) equals: dog breed.
! !

!DogWebServerTests methodsFor: 'testing' stamp: 'RMV 10/12/2024 15:41:03'!
testHeadDogs
	| contentLength res url |

	url := self baseUrl.

	"Send a GET request in order to compute the expected content length."
	res := WebClientPlus method: #GET url: url.	
	self assert: res code equals: 200.
	contentLength := res content size.

	"Send a HEAD request."
	res := WebClientPlus method: #HEAD url: url.	
	self assert: res code equals: 200.
	self assert: (res headerAt: 'Content-Type') equals: WebServerPlus jsonContentType.
	self assert: (res headerAt: 'Content-Length') equals: contentLength asString.
	self assert: res content equals: ''.
	! !

!DogWebServerTests methodsFor: 'testing' stamp: 'RMV 10/9/2024 15:57:56'!
testHello
	| res |

	res := WebClientPlus method: #GET url: 'http://localhost:3000/hello'.
	self assert: res code equals: 200.
	self assert: res content equals: 'Hello, World!!'.
! !

!DogWebServerTests methodsFor: 'testing' stamp: 'RMV 10/12/2024 14:15:47'!
testPatch
	| baseUrl dog dogs json jsonArray name res url |

	"Get any one of the current dogs."
	baseUrl := self baseUrl.
	res := WebClientPlus method: #GET url: baseUrl.
	res code print.
	self assert: res code equals: 200.
	jsonArray := Json readFrom: res stream.
	dogs := jsonArray collect: [ :jsonObject | Dog fromJsonObject: jsonObject ].
	dog := dogs first.

	"Modify the name of that dog."
	url := '{1}/{2}' format: {baseUrl. dog id}.
	name := 'Spot'.
	json := {#name->name} asDictionary asJsonString.
	res := WebClientPlus method: #PATCH url: url content: json.
	self assert: res code equals: 200.
	json := Json readFrom: res stream.
	json print.
	"self assert: (json at: #name) equals: name.
	self assert: (json at: #breed) equals: dog breed."
	
	"Get the modified dog."
	res := WebClientPlus method: #GET url: url.
	self assert: res code equals: 200.
	json := Json readFrom: res stream.
	self assert: (json at: #name) equals: name.
	self assert: (json at: #breed) equals: dog breed.
! !

!DogWebServerTests methodsFor: 'testing' stamp: 'RMV 10/12/2024 15:41:30'!
testPost
	| baseUrl breed id json name res url |

	"Create a new dog."		
	baseUrl := self baseUrl.
	name := 'Snoopy'.
	breed := 'Beagle'.
	json := {#name->name. #breed->breed} asDictionary asJsonString.
	res := WebClientPlus method: #POST url: baseUrl content: json.
	self assert: res code equals: 201.
	json := Json readFrom: res stream.
	self assert: (json at: #name) equals: name.
	self assert: (json at: #breed) equals: breed.
	id := json at: #id.
	
	"Get the dog that was just created."
	url := '{1}/{2}' format: {baseUrl. id}.
	res := WebClientPlus method: #GET url: url.
	self assert: res code equals: 200.
	json := Json readFrom: res stream.
	self assert: (json at: #name) equals: name.
	self assert: (json at: #breed) equals: breed.
! !

!DogWebServerTests methodsFor: 'testing' stamp: 'RMV 10/9/2024 18:58:35'!
testPut
	| baseUrl breed dog dogs json jsonArray name res url |

	"Get any one of the current dogs."
	baseUrl := self baseUrl.
	res := WebClientPlus method: #GET url: baseUrl.
	self assert: res code equals: 200.
	jsonArray := Json readFrom: res stream.
	dogs := jsonArray collect: [ :jsonObject | Dog fromJsonObject: jsonObject ].
	dog := dogs first.

	"Modify the name and breed of that dog."
	url := '{1}/{2}' format: {baseUrl. dog id}.
	name := 'Fireball'.
	breed := 'Greyhound'.
	json := {#name->name. #breed->breed} asDictionary asJsonString.
	res := WebClientPlus method: #PUT url: url content: json.
	self assert: res code equals: 200.
	json := Json readFrom: res stream.
	self assert: (json at: #name) equals: name.
	self assert: (json at: #breed) equals: breed.
	
	"Get the modified dog."
	res := WebClientPlus method: #GET url: url.
	self assert: res code equals: 200.
	json := Json readFrom: res stream.
	self assert: (json at: #name) equals: name.
	self assert: (json at: #breed) equals: breed.
! !

!DogWebServerTests methodsFor: 'accessing' stamp: 'RMV 10/9/2024 18:38:31'!
baseUrl
	^ 'http://localhost:3000/dog'! !

!HTMLTests methodsFor: 'as yet unclassified' stamp: 'RMV 10/13/2024 18:39:40'!
testElMethods
	| attrs children name |
	
	name := 'demo'.
	attrs := Dictionary newFrom: {'a1' -> 1. 'a2' -> 2}.
	children := { HTML el: 'c1' children: 'Hello'. HTML el: 'c2' children: 'World!!' }.

	self assert: (HTML elc: name) equals: '<demo>'.
	self assert: (HTML elc: name attrs: attrs) equals: '<demo a1="1" a2="2">'.
	self assert: (HTML el: name) equals: '<demo></demo>'.
	self assert: (HTML el: name attrs: attrs) equals: '<demo a1="1" a2="2"></demo>'.
	self assert: (HTML el: name children: 'content') equals: '<demo>content</demo>'.
	self assert: (HTML el: name children: children) equals: '<demo><c1>Hello</c1><c2>World!!</c2></demo>'.
	self assert: (HTML el: name attrs: attrs children: 'content') equals: '<demo a1="1" a2="2">content</demo>'.
	self assert: (HTML el: name attrs: attrs children: children) equals: '<demo a1="1" a2="2"><c1>Hello</c1><c2>World!!</c2></demo>'.! !

!HTMLTests methodsFor: 'as yet unclassified' stamp: 'RMV 10/13/2024 18:39:40'!
testGeneratedMethods
	| aAttrs children imgAttrs text ulAttrs |
	
	"Self-closing element with no attributes"
	self assert: (HTML br) equals: '<br>'.

	"Self-closing element with attributes"
	imgAttrs := Dictionary newFrom: {'src' -> 'dog.png'. 'class' -> 'petImage'}.
	self assert: (HTML imgAttrs: imgAttrs) equals: '<img class="petImage" src="dog.png">'.
	
	"Regular element with text content"
	text := 'Dog'.
	self assert: (HTML pChildren: text) equals: '<p>Dog</p>'.
	
	"Regular element with attributes and text content"
	aAttrs := Dictionary newFrom: {'href' -> 'dog.html'. 'target' -> '_blank'}.
	self assert: (HTML aAttrs: aAttrs children: text) equals: '<a href="dog.html" target="_blank">Dog</a>'.
	
	"Regular element with attributes and child elements"
	ulAttrs := Dictionary newFrom: {'class' -> 'special'}.
	children := { HTML liChildren: 'first'. HTML liChildren: 'second' }.
	self assert: (HTML ulAttrs: ulAttrs children: children) equals: '<ul class="special"><li>first</li><li>second</li></ul>'.
! !

!WebClientPlus class methodsFor: 'as yet unclassified' stamp: 'RMV 10/8/2024 18:29:51'!
method: methodSymbol url: urlString
	^ self method: methodSymbol url: urlString headers: nil content: nil
! !

!WebClientPlus class methodsFor: 'as yet unclassified' stamp: 'RMV 10/8/2024 17:12:16'!
method: methodSymbol url: urlString content: contentString
	^ self method: methodSymbol url: urlString headers: nil content: contentString
! !

!WebClientPlus class methodsFor: 'as yet unclassified' stamp: 'RMV 10/8/2024 17:13:35'!
method: methodSymbol url: urlString headers: headerDict
	^ self method: methodSymbol url: urlString headers: headerDict content: nil! !

!WebClientPlus class methodsFor: 'as yet unclassified' stamp: 'RMV 10/9/2024 15:39:02'!
method: methodSymbol url: urlString headers: headerDict content: contentString
	| client req res |
	
	client := WebClient new.
	"I don't understand why urlString needs to be passed to two methods."
	client initializeFromUrl: urlString.
	req := client requestWithUrl: urlString.
	
	req method: methodSymbol.
	headerDict ifNotNil: [
		headerDict associationsDo: [ :assoc | req headerAt: assoc key put: assoc value ]
	].

	"TODO: Add error handling here to handle when the server is not running."
	res := contentString
		ifNil: [ client sendRequest: req ]
		ifNotNil: [ client sendRequest: req content: contentString readStream size: contentString size ].
	^res.
! !

!WebServerPlus methodsFor: 'accessing' stamp: 'RMV 10/13/2024 12:09:32'!
staticFilePath
	^staticFilePath! !

!WebServerPlus methodsFor: 'accessing' stamp: 'RMV 10/13/2024 12:03:09'!
staticFilePath: filePathString
	staticFilePath := filePathString! !

!WebServerPlus methodsFor: 'html' stamp: 'RMV 10/7/2024 11:15:37'!
element: anAssoc
	"This returns an HTML string.
	The key in the Association must be a kind of String (such as a Symbol).
	The value in the Association can be a primitive value or
	an Array of Associations that represent nested elements."
	
	| content inner tag template |
	
	tag := anAssoc key.
	content := anAssoc value.
	"TODO: Add support for attributes."
	template := '<{1}>{2}</{1}>'.
	(content isKindOf: String)
		ifTrue: [^​ template format: { tag. content } ]
		ifFalse: [
			"Maybe this should raise an exception if (content isKindOf: SequenceableCollection) is false."
			inner := ''.
			content do: [ :item | inner := inner, (self element: item) ].
			^ template format: { tag. inner }
		]
! !

!WebServerPlus methodsFor: 'initialization' stamp: 'RMV 10/13/2024 11:50:11'!
initialize
	super initialize.
	
	dynamicRoutes := OrderedCollection new.
	options := Dictionary new.
	staticFilePath := ''.
	staticRoutes := Dictionary new.
	
	"This handles all requests."
	action := [:request |
		| route |
		"'got request for ', request method, ': ', request url :: print."
		route := self routeForMethod: request method path: request url.
		route
			ifNotNil: [
				| context handler |
				context := WebContext newRequest: request route: route.
				handler := route handler.
				handler isSymbol ifTrue: [ self perform: handler with: context ].
				handler isBlock ifTrue: [ handler value: context ].
				"self send400Response: 'handler type ', handler class name, ' not supported'."
			]
			ifNil: [ request send404Response ].
		]	! !

!WebServerPlus methodsFor: 'routing' stamp: 'RMV 10/13/2024 12:07:54'!
dispatchRequest: request url: url
	"This overrides the method in WebServer to assume
	only one action that handles all requests.
	It is defined in the initialize method."
	| pathOptions |

	"Handle TRACE requests"
	request method = 'TRACE' ifTrue: [
		^request send200Response: request asString contentType: 'message/http'.
	].

	"Determine the HTTP methods supported for the URL."
	pathOptions := options at: url ifAbsent: [
		dynamicRoutes
			select: [:route | route pattern match: url] ::
			collect: [:route | route method ]
	].
	pathOptions ifNotEmpty: [
		"These HTTP methods are supported for every URL."
		#(#HEAD #OPTIONS #TRACE) do: [:method | pathOptions add: method].
	].

	"Handle OPTIONS requests"
	request method = 'OPTIONS' ifTrue: [
		pathOptions
			ifNotEmpty: [ request sendOptionsResponse: pathOptions asArray ]
			ifEmpty: [ request send404Response ]
	].

	"If the path ends with a file extension, attempt to serve a static file."
	url includesSubString: '.' ::
		ifTrue: [
			| body contentType fileEntry filePath |
			filePath := url copyFrom: 2 to: url size. "removes leading slash"
			staticFilePath isEmpty ifFalse: [ filePath := staticFilePath, '/', filePath ].
			fileEntry := filePath asFileEntry.
			[
				body := fileEntry fileContents.
				contentType := MimeType forFileExtension: fileEntry extension.
				^request send200Response: body contentType: contentType.
			] on: FileDoesNotExistException do: [
				^request send404Response.
			]
		].
		
	"Handle 405 Method Not Allowed"
	pathOptions includes: request method :: ifFalse: [
		^request send405Response: pathOptions asArray.
	].

	^[self invokeAction: action request: request] 
		on: Error 
		do: [ :ex |
			errorHandler 
				ifNil: [ self handleError: ex request: request ]
				ifNotNil: [ errorHandler value: ex value: request ]
		].! !

!WebServerPlus methodsFor: 'routing' stamp: 'RMV 10/9/2024 15:38:14'!
method: methodSymbol path: pathString handler: handler
	"Registers a new route.
	handler can be a method selector or a block.
	The method or block will be passed a WebContext object."
	| isDynamic pathOptions route |
	
	route := WebRoute method: methodSymbol path: pathString handler: handler.
	isDynamic := pathString includesSubString: '/:'.
	isDynamic
		ifTrue: [ dynamicRoutes add: route ]
		ifFalse: [ staticRoutes at: route key put: route ].
	
	pathOptions := options at: pathString ifAbsent: [
		| set |
		set := Set new.
		options at: pathString put: set.
		set
	].	
	pathOptions add: methodSymbol.! !

!WebServerPlus methodsFor: 'routing' stamp: 'RMV 10/8/2024 09:19:46'!
routeForMethod: methodSymbol path: pathString
	| key |
	
	key := methodSymbol, ': ', pathString.
	^ staticRoutes at: key ifAbsent: [
		dynamicRoutes
			detect: [:each |
				methodSymbol = each method and: [each pattern match: pathString]
			]
			ifNone: [ nil ].
	]! !

!WebServerPlus class methodsFor: 'as yet unclassified' stamp: 'RMV 10/9/2024 18:36:00'!
jsonContentType
	^ 'application/json; charset=utf-8'! !

!DogWebServer methodsFor: 'initialization' stamp: 'RMV 10/9/2024 18:58:02'!
initialize
	| dog1 dog2 |
		
	super initialize.
	
	"Create some initial dogs."
	dog1 := Dog name: 'Comet' breed: 'Whippet'.
	dog2 := Dog name: 'Oscar' breed: 'German Shorthaired Pointer'.
	dogDict := Dictionary newFrom: {
		dog1 id -> dog1.
		dog2 id -> dog2
	}.
	
	"Register routes."
	self method: #GET path: '/hello' handler: [ :context |
		context request send200Response: 'Hello, World!!'
	].
	"Don't forget the colon the end of the selectors!!"
	self method: #GET path: '/dog' handler: #getDogs:.
	"Only supporting HEAD requests for /dog currently, just to show how it is done."
	self method: #HEAD path: '/dog' handler: #headDogs:.
	self method: #GET path: '/dog/:id' handler: #getDogAsJson:.
	self method: #POST path: '/dog' handler: #createDog:.
	self method: #PATCH path: '/dog/:id' handler: #patchDog:.
	self method: #PUT path: '/dog/:id' handler: #updateDog:.
	self method: #DELETE path: '/dog/:id' handler: #deleteDog:.
	self method: #DELETE path: '/dog' handler: #deleteAllDogs:.! !

!DogWebServer methodsFor: 'private' stamp: 'RMV 10/8/2024 20:34:27'!
deleteAllDogs: aWebContext
	"This should only be used by the setUp method in DogWebServerTests."
	| req |
	
	req := aWebContext request.

	dogDict := Dictionary new.

	"Return an empty response."
	^ req send200Response: ''
! !

!DogWebServer methodsFor: 'message handling' stamp: 'RMV 10/12/2024 19:33:54'!
createDog: aWebContext
 	| dog req res |
	
	req := aWebContext request.
	
	"Get Dog object from request body. An id will be assigned."
	dog := Dog fromJson: req stream.
	
	"Save the new dog."
	dogDict at: dog id put: dog.
	
	"Return the new dog as JSON."
	res := WebResponse protocol: req protocol code: 201. "Created"
	res headerAt: 'Content-Type' put: WebServerPlus jsonContentType.
	^ req sendResponse: res content: (Json render: dog)

! !

!DogWebServer methodsFor: 'message handling' stamp: 'RMV 9/26/2024 20:18:01'!
deleteDog: aWebContext
	| id req |
	
	req := aWebContext request.
	id := aWebContext pathParameter: #id.
	id := [id asNumber] on: Error do: [ :e | ^ req send400Response: e messageText].
	
	"Delete the dog, verifying that a dog with the given id exists."
	dogDict removeKey: id ifAbsent: [
		^ req send404Response.
	].

	"Return an empty response."
	^ req send200Response: ''
! !

!DogWebServer methodsFor: 'message handling' stamp: 'RMV 10/12/2024 19:33:11'!
getDogAsJson: aWebContext
	| dog id req |
	
	req := aWebContext request.
	id := aWebContext pathParameter: #id.
	id := [id asNumber] on: Error do: [ :e | ^ req send400Response: e messageText].
	
	dog := dogDict at: id ifAbsent: nil.
	dog
		ifNil: [ req send404Response ]
		ifNotNil: [
			req
				send200Response: (Json render: dog)
				contentType: WebServerPlus jsonContentType
		].	
! !

!DogWebServer methodsFor: 'message handling' stamp: 'RMV 10/12/2024 19:42:25'!
getDogs: aWebContext	
	| accept queryParams req |
	
	"This just demonstrates that we can get query parameters."
	queryParams := aWebContext queryParameters.
	queryParams logAs: 'queryParams'.
	
	req := aWebContext request.
	accept := req headerAt: 'Accept'.
	"Only return HTML if requested in the Accept header.
	Otherwise default to returning JSON."
	accept includesSubString: 'text/html' ::
		ifTrue: [^self getDogsAsHtml: req]
		ifFalse: [^self getDogsAsJson: req]! !

!DogWebServer methodsFor: 'message handling' stamp: 'RMV 9/26/2024 20:08:43'!
getDogsAsHtml: aRequest
	| css html tableRows |
	
	css := '
		body { font-family: sans-serif }
		table { border-collapse: collapse }
		td, th {
			border: 1px solid gray;
			padding: 0.5rem;
		}
		th {
			background-color: pink;
		}
	'.
	
	tableRows := OrderedCollection new.
	tableRows add: #tr -> { #th -> 'Name'. #th -> 'Breed' }.
	dogDict do: [ :dog | tableRows add: (#tr -> { #td -> dog name. #td -> dog breed }) ].
	
	html := self element: (#html -> {
		#head -> {
			#title -> 'My Dogs'.
			#style -> css
		}.
		#body -> {
			#h1 -> 'My Dogs'.
			#table -> tableRows
		}
	}).
		
	aRequest send200Response: html contentType: 'text/html'
! !

!DogWebServer methodsFor: 'message handling' stamp: 'RMV 10/9/2024 18:36:37'!
getDogsAsJson: aRequest
	| json |
		
	"Create a JSON array from the dog Dictionary."
	json := Json render: dogDict asArray.
	
	aRequest send200Response: json contentType: WebServerPlus jsonContentType.
	
! !

!DogWebServer methodsFor: 'message handling' stamp: 'RMV 10/9/2024 18:50:59'!
headDogs: aWebContext	
	"Returns the same headers as a corresponding GET request,
	but does not return any body content."
	| json req |
	
	json := Json render: dogDict asArray.
	req := aWebContext request.
	
	"The method is already set to HEAD at this point.
	When the method is HEAD, the content is not written to the response body,
	but it is used to compute the Content-Length header value."
	req send200Response: json contentType: WebServerPlus jsonContentType do: [ :res |
		res headerAt: 'X-Verify' put: 'verifies ability to set a custom header'.
	].
! !

!DogWebServer methodsFor: 'message handling' stamp: 'RMV 10/12/2024 19:34:06'!
patchDog: aWebContext
	| dog id jsonObject req |
	
	req := aWebContext request.
	id := aWebContext pathParameter: #id.
	id := [id asNumber] on: Error do: [ :e | ^ req send400Response: e messageText].

	"Verify that a dog with the given id exists."
	dog := dogDict at: id ifAbsent: [^ req send404Response].

	"Get JSON from request body."
	jsonObject := Json readFrom: req stream.

	"Patch the dog object."
	jsonObject associationsDo: [:assoc |
		| selector |
		selector := assoc key, ':' :: asSymbol.
		dog perform: selector with: assoc value.
	].
	dogDict at: id put: dog.
		
	"Return the updated dog as JSON."
	^ req send200Response: (Json render: dog) contentType: WebServerPlus jsonContentType
! !

!DogWebServer methodsFor: 'message handling' stamp: 'RMV 10/12/2024 19:34:14'!
updateDog: aWebContext
	| dog id req |
	
	req := aWebContext request.
	id := aWebContext pathParameter: #id.
	id := [id asNumber] on: Error do: [ :e | ^ req send400Response: e messageText].

	"Verify that a dog with the given id exists."
	dogDict at: id ifAbsent: [^ req send404Response].

	"Get Dog object from request body."
	dog := Dog fromJson: req stream.
	
	"Update its id to match id parameter."
	dog id: id.
	
	"Save the change."
	dogDict at: dog id put: dog.
	
	"Return the updated dog as JSON."
	^ req send200Response: (Json render: dog) contentType: WebServerPlus jsonContentType
! !

!Dog methodsFor: 'initialization' stamp: 'RMV 6/12/2024 10:47:34'!
initialize
	super initialize.
	Count := Count + 1! !

!Dog methodsFor: 'private' stamp: 'RMV 6/17/2024 15:18:17'!
id: aNumber
	"used by MyWebServer deleteDog method"
	id := aNumber! !

!Dog methodsFor: 'private' stamp: 'RMV 6/23/2024 09:46:32'!
setId: anId name: aName breed: aBreed
	id := anId.
	name := aName.
	breed := aBreed! !

!Dog methodsFor: 'accessing' stamp: 'RMV 10/9/2024 18:57:49'!
= otherDog
	"Answer whether this Dog has same name and breed as otherDog.
	It does not compare their ids."

	^ name = otherDog name and: [breed = otherDog breed]! !

!Dog methodsFor: 'accessing' stamp: 'RMV 7/1/2024 20:42:14'!
breed
	"Answers the breed of the dog as a String."
	^ breed.! !

!Dog methodsFor: 'accessing' stamp: 'RMV 8/1/2024 10:06:37'!
breed: aString
	breed := aString.! !

!Dog methodsFor: 'accessing' stamp: 'RMV 6/16/2024 16:06:12'!
id
	^id! !

!Dog methodsFor: 'accessing' stamp: 'RMV 7/1/2024 20:42:35'!
name
	"Answers the name of the dog as a String."
	^ name.! !

!Dog methodsFor: 'accessing' stamp: 'RMV 8/1/2024 10:06:24'!
name: aString
	name := aString! !

!Dog methodsFor: 'accessing' stamp: 'RMV 10/8/2024 14:58:27'!
printString
	^ name, ' is a ', breed! !

!Dog methodsFor: 'converting' stamp: 'RMV 7/21/2024 08:44:34'!
jsonWriteOn: aWriteStream
	"writes a JSON representation of this object to a given stream"
	{#id -> id. #name -> name. #breed -> breed} asDictionary jsonWriteOn: aWriteStream.! !

!Dog methodsFor: 'error handling' stamp: 'RMV 10/9/2024 15:22:24'!
doesNotUnderstand: aMessage
	(aMessage handler = 'bark:') ifTrue: [
		| count |
		count := aMessage arguments first.
		count isNumber ifTrue: [ ('Woof!! ' repeat: count) print. ^ nil ]
	].
	super doesNotUnderstand: aMessage! !

!Dog class methodsFor: 'accessing' stamp: 'RMV 6/17/2024 14:47:58'!
nextId
	LastId := LastId + 1.
	^LastId! !

!Dog class methodsFor: 'initialization' stamp: 'RMV 6/23/2024 09:46:32'!
id: anId name: aName breed: aBreed
	^self new setId: anId name: aName breed: aBreed! !

!Dog class methodsFor: 'initialization' stamp: 'RMV 10/9/2024 18:57:41'!
initialize
	"This must be explicitly called with Dog initialize."
	Count := 0.
	LastId := 0.
	'initialized Dog class' print! !

!Dog class methodsFor: 'initialization' stamp: 'RMV 10/9/2024 18:57:44'!
name: aName breed: aBreed
	^self new setId: (Dog nextId) name: aName breed: aBreed! !

!Dog class methodsFor: 'converting' stamp: 'RMV 10/8/2024 14:57:07'!
fromJson: aStream
	^self fromJsonObject: (Json readFrom: aStream)
! !

!Dog class methodsFor: 'converting' stamp: 'RMV 10/9/2024 18:57:37'!
fromJsonObject: jsonObject
	| newId oldId |
	oldId := jsonObject at: #id.
	newId := oldId isNil ifTrue: [ Dog nextId ] ifFalse: [ oldId ].
	^Dog id: newId name: (jsonObject at: #name) breed: (jsonObject at: #breed).
! !

!HTML class methodsFor: 'initialization' stamp: 'RMV 10/13/2024 18:43:26'!
initialize
	| contentElements newLine selfClosingElements tab |
	
	newLine := Character newLineCharacter asString.
	tab := Character tab asString.

	contentElements := #(
  		#a
   		#article
   		#aside
   		#audio
   		#b
   		#body
   		#blockquote
   		#button
   		#canvas
   		#caption
   		#cite
   		#code
   		#colgroup
   		#dd
   		#details
   		#dialog
   		#div
   		#dl
   		#dt
   		#em
   		#fieldset
   		#figcaption
   		#figure
   		#form
   		#h1
   		#h2
   		#h3
   		#h4
  		#h5
   		#h6
   		#head
   		#header
   		#html
   		#i
   		#iframe
   		#label
   		#li
   		#main
   		#map
   		#mark
   		#menu
   		#nav
   		#object
   		#ol
   		#optgroup
   		#option
   		#output
   		#p
   		#picture
   		#pre
   		#progress
   		#q
   		#script
   		#section
   		#select
   		#slot
   		#small
   		#span
   		#strong
   		#style
   		#sub
   		#summary
   		#sup
   		#table
   		#tbody
   		#td
   		#template
   		#textarea
   		#tfoot
   		#th
   		#thead
   		#time
   		#title
   		#tr
   		#u
   		#ul
   		#var
   		#video
	).
	
	contentElements do: [:el |
		| source |
		
		"Generate a method that takes a children Array."
		source := 	'{1}Children: children{2}{3}^HTML el: ''{1}'' children: children' format: {el. newLine. tab }.
		self class compile: source classified: 'dynamically added'.
		
		"Generate a method that takes an attributes Dictionary and a children Array."
		source := 	'{1}Attrs: attrs children: children{2}{3}^HTML el: ''{1}'' attrs: attrs children: children' format: {el. newLine. tab}.
		self class compile: source classified: 'dynamically added'.
	].

	selfClosingElements := #(
     		#area
     		#base
     		#br
     		#col
     		#embed
     		#hr
     		#img
     		#input
     		#link
     		#meta
     		#source
     		#track
     		#wbr
	).
	
	selfClosingElements do: [:elc |
		| source |
		
		"Generate a method that takes no arguments."
		source := 	'{1}{2}{3}^HTML elc: ''{1}''' format: {elc. newLine. tab}.
		self class compile: source classified: 'dynamically added'.
		
		"Generate a method that takes an attributes Dictionary."
		source := 	'{1}Attrs: attrs{2}{3}^HTML elc: ''{1}'' attrs: attrs' format: {elc. newLine. tab}.
		self class compile: source classified: 'dynamically added'.
	].
! !

!HTML class methodsFor: 'html generation' stamp: 'RMV 10/13/2024 16:44:56'!
el: nameString
	^ self el: nameString attrs: nil children: ''
! !

!HTML class methodsFor: 'html generation' stamp: 'RMV 10/13/2024 16:45:06'!
el: nameString attrs: attrsDictionary
	^ self el: nameString attrs: attrsDictionary children: ''.
! !

!HTML class methodsFor: 'html generation' stamp: 'RMV 10/13/2024 16:44:37'!
el: nameString attrs: attrsDictionary children: childArray
	"Answer HTML string for element that is not self-closing."
	| attrs text |
	
	attrs := ''.
	attrsDictionary ifNotNil: [
		| keys |
		keys := attrsDictionary keys sort.
		keys do: [:key |
			attrs := '{1} {2}="{3}"' format: {attrs. key. attrsDictionary at: key}.
		].
	].

	childArray isString
		ifTrue: [ text := childArray ]
		ifFalse: [
			text := ''.
			childArray ifNotNil: [
				childArray do: [:child | text := text, child]
			]
	].

	^ '<{1}{2}>{3}</{1}>' format: {nameString. attrs. text}.	! !

!HTML class methodsFor: 'html generation' stamp: 'RMV 9/29/2024 11:08:52'!
el: nameString children: childArray
	^ self el: nameString attrs: nil children: childArray
! !

!HTML class methodsFor: 'html generation' stamp: 'RMV 10/13/2024 15:19:36'!
elc: nameString
	"Answer HTML string for a self-closing element with no attributes."
	^ self elc: nameString attrs: nil
! !

!HTML class methodsFor: 'html generation' stamp: 'RMV 10/13/2024 16:03:27'!
elc: nameString attrs: attrsDictionary
	"Answer HTML string for a self-closing element with attributes."
	| attrs |
	
	attrs := ''.	
	attrsDictionary ifNotNil: [
		| keys |
		keys := attrsDictionary keys sort.
		keys do: [:key |
			attrs := '{1} {2}="{3}"' format: {attrs. key. attrsDictionary at: key}.
		].
	].
	
	^ '<{1}{2}>' format: {nameString. attrs}.	! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
aAttrs: attrs children: children
	^HTML el: 'a' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
aChildren: children
	^HTML el: 'a' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
area
	^HTML elc: 'area'! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
areaAttrs: attrs
	^HTML elc: 'area' attrs: attrs! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
articleAttrs: attrs children: children
	^HTML el: 'article' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
articleChildren: children
	^HTML el: 'article' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
asideAttrs: attrs children: children
	^HTML el: 'aside' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
asideChildren: children
	^HTML el: 'aside' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
audioAttrs: attrs children: children
	^HTML el: 'audio' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
audioChildren: children
	^HTML el: 'audio' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
bAttrs: attrs children: children
	^HTML el: 'b' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
bChildren: children
	^HTML el: 'b' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
base
	^HTML elc: 'base'! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
baseAttrs: attrs
	^HTML elc: 'base' attrs: attrs! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
blockquoteAttrs: attrs children: children
	^HTML el: 'blockquote' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
blockquoteChildren: children
	^HTML el: 'blockquote' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
bodyAttrs: attrs children: children
	^HTML el: 'body' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
bodyChildren: children
	^HTML el: 'body' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
br
	^HTML elc: 'br'! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
brAttrs: attrs
	^HTML elc: 'br' attrs: attrs! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
buttonAttrs: attrs children: children
	^HTML el: 'button' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
buttonChildren: children
	^HTML el: 'button' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
canvasAttrs: attrs children: children
	^HTML el: 'canvas' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
canvasChildren: children
	^HTML el: 'canvas' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
captionAttrs: attrs children: children
	^HTML el: 'caption' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
captionChildren: children
	^HTML el: 'caption' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
citeAttrs: attrs children: children
	^HTML el: 'cite' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
citeChildren: children
	^HTML el: 'cite' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
codeAttrs: attrs children: children
	^HTML el: 'code' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
codeChildren: children
	^HTML el: 'code' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
col
	^HTML elc: 'col'! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
colAttrs: attrs
	^HTML elc: 'col' attrs: attrs! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
colgroupAttrs: attrs children: children
	^HTML el: 'colgroup' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
colgroupChildren: children
	^HTML el: 'colgroup' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
ddAttrs: attrs children: children
	^HTML el: 'dd' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
ddChildren: children
	^HTML el: 'dd' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
detailsAttrs: attrs children: children
	^HTML el: 'details' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
detailsChildren: children
	^HTML el: 'details' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
dialogAttrs: attrs children: children
	^HTML el: 'dialog' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
dialogChildren: children
	^HTML el: 'dialog' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
divAttrs: attrs children: children
	^HTML el: 'div' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
divChildren: children
	^HTML el: 'div' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
dlAttrs: attrs children: children
	^HTML el: 'dl' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
dlChildren: children
	^HTML el: 'dl' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
dtAttrs: attrs children: children
	^HTML el: 'dt' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
dtChildren: children
	^HTML el: 'dt' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
emAttrs: attrs children: children
	^HTML el: 'em' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
emChildren: children
	^HTML el: 'em' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
embed
	^HTML elc: 'embed'! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
embedAttrs: attrs
	^HTML elc: 'embed' attrs: attrs! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
fieldsetAttrs: attrs children: children
	^HTML el: 'fieldset' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
fieldsetChildren: children
	^HTML el: 'fieldset' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
figcaptionAttrs: attrs children: children
	^HTML el: 'figcaption' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
figcaptionChildren: children
	^HTML el: 'figcaption' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
figureAttrs: attrs children: children
	^HTML el: 'figure' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
figureChildren: children
	^HTML el: 'figure' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
formAttrs: attrs children: children
	^HTML el: 'form' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
formChildren: children
	^HTML el: 'form' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
h1Attrs: attrs children: children
	^HTML el: 'h1' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
h1Children: children
	^HTML el: 'h1' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
h2Attrs: attrs children: children
	^HTML el: 'h2' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
h2Children: children
	^HTML el: 'h2' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
h3Attrs: attrs children: children
	^HTML el: 'h3' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
h3Children: children
	^HTML el: 'h3' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
h4Attrs: attrs children: children
	^HTML el: 'h4' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
h4Children: children
	^HTML el: 'h4' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
h5Attrs: attrs children: children
	^HTML el: 'h5' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
h5Children: children
	^HTML el: 'h5' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
h6Attrs: attrs children: children
	^HTML el: 'h6' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
h6Children: children
	^HTML el: 'h6' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
headAttrs: attrs children: children
	^HTML el: 'head' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
headChildren: children
	^HTML el: 'head' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
headerAttrs: attrs children: children
	^HTML el: 'header' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
headerChildren: children
	^HTML el: 'header' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
hr
	^HTML elc: 'hr'! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
hrAttrs: attrs
	^HTML elc: 'hr' attrs: attrs! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
htmlAttrs: attrs children: children
	^HTML el: 'html' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
htmlChildren: children
	^HTML el: 'html' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
iAttrs: attrs children: children
	^HTML el: 'i' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
iChildren: children
	^HTML el: 'i' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
iframeAttrs: attrs children: children
	^HTML el: 'iframe' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
iframeChildren: children
	^HTML el: 'iframe' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
img
	^HTML elc: 'img'! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
imgAttrs: attrs
	^HTML elc: 'img' attrs: attrs! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
input
	^HTML elc: 'input'! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
inputAttrs: attrs
	^HTML elc: 'input' attrs: attrs! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
labelAttrs: attrs children: children
	^HTML el: 'label' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
labelChildren: children
	^HTML el: 'label' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
liAttrs: attrs children: children
	^HTML el: 'li' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
liChildren: children
	^HTML el: 'li' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
link
	^HTML elc: 'link'! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
linkAttrs: attrs
	^HTML elc: 'link' attrs: attrs! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
mainAttrs: attrs children: children
	^HTML el: 'main' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
mainChildren: children
	^HTML el: 'main' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
mapAttrs: attrs children: children
	^HTML el: 'map' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
mapChildren: children
	^HTML el: 'map' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
markAttrs: attrs children: children
	^HTML el: 'mark' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
markChildren: children
	^HTML el: 'mark' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
menuAttrs: attrs children: children
	^HTML el: 'menu' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
menuChildren: children
	^HTML el: 'menu' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
meta
	^HTML elc: 'meta'! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
metaAttrs: attrs
	^HTML elc: 'meta' attrs: attrs! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
navAttrs: attrs children: children
	^HTML el: 'nav' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
navChildren: children
	^HTML el: 'nav' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
objectAttrs: attrs children: children
	^HTML el: 'object' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
objectChildren: children
	^HTML el: 'object' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
olAttrs: attrs children: children
	^HTML el: 'ol' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
olChildren: children
	^HTML el: 'ol' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
optgroupAttrs: attrs children: children
	^HTML el: 'optgroup' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
optgroupChildren: children
	^HTML el: 'optgroup' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
optionAttrs: attrs children: children
	^HTML el: 'option' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
optionChildren: children
	^HTML el: 'option' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
outputAttrs: attrs children: children
	^HTML el: 'output' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
outputChildren: children
	^HTML el: 'output' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
pAttrs: attrs children: children
	^HTML el: 'p' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
pChildren: children
	^HTML el: 'p' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
pictureAttrs: attrs children: children
	^HTML el: 'picture' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
pictureChildren: children
	^HTML el: 'picture' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
preAttrs: attrs children: children
	^HTML el: 'pre' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
preChildren: children
	^HTML el: 'pre' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
progressAttrs: attrs children: children
	^HTML el: 'progress' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
progressChildren: children
	^HTML el: 'progress' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
qAttrs: attrs children: children
	^HTML el: 'q' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
qChildren: children
	^HTML el: 'q' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
scriptAttrs: attrs children: children
	^HTML el: 'script' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
scriptChildren: children
	^HTML el: 'script' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
sectionAttrs: attrs children: children
	^HTML el: 'section' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
sectionChildren: children
	^HTML el: 'section' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
selectAttrs: attrs children: children
	^HTML el: 'select' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
selectChildren: children
	^HTML el: 'select' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
slotAttrs: attrs children: children
	^HTML el: 'slot' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
slotChildren: children
	^HTML el: 'slot' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
smallAttrs: attrs children: children
	^HTML el: 'small' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
smallChildren: children
	^HTML el: 'small' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
source
	^HTML elc: 'source'! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
sourceAttrs: attrs
	^HTML elc: 'source' attrs: attrs! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
spanAttrs: attrs children: children
	^HTML el: 'span' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
spanChildren: children
	^HTML el: 'span' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
strongAttrs: attrs children: children
	^HTML el: 'strong' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
strongChildren: children
	^HTML el: 'strong' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
styleAttrs: attrs children: children
	^HTML el: 'style' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
styleChildren: children
	^HTML el: 'style' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
subAttrs: attrs children: children
	^HTML el: 'sub' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
subChildren: children
	^HTML el: 'sub' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
summaryAttrs: attrs children: children
	^HTML el: 'summary' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
summaryChildren: children
	^HTML el: 'summary' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
supAttrs: attrs children: children
	^HTML el: 'sup' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
supChildren: children
	^HTML el: 'sup' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
tableAttrs: attrs children: children
	^HTML el: 'table' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
tableChildren: children
	^HTML el: 'table' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
tbodyAttrs: attrs children: children
	^HTML el: 'tbody' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
tbodyChildren: children
	^HTML el: 'tbody' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
tdAttrs: attrs children: children
	^HTML el: 'td' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
tdChildren: children
	^HTML el: 'td' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
templateAttrs: attrs children: children
	^HTML el: 'template' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
templateChildren: children
	^HTML el: 'template' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
textareaAttrs: attrs children: children
	^HTML el: 'textarea' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
textareaChildren: children
	^HTML el: 'textarea' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
tfootAttrs: attrs children: children
	^HTML el: 'tfoot' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
tfootChildren: children
	^HTML el: 'tfoot' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
thAttrs: attrs children: children
	^HTML el: 'th' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
thChildren: children
	^HTML el: 'th' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
theadAttrs: attrs children: children
	^HTML el: 'thead' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
theadChildren: children
	^HTML el: 'thead' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
timeAttrs: attrs children: children
	^HTML el: 'time' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
timeChildren: children
	^HTML el: 'time' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
titleAttrs: attrs children: children
	^HTML el: 'title' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
titleChildren: children
	^HTML el: 'title' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
trAttrs: attrs children: children
	^HTML el: 'tr' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
trChildren: children
	^HTML el: 'tr' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
track
	^HTML elc: 'track'! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
trackAttrs: attrs
	^HTML elc: 'track' attrs: attrs! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
uAttrs: attrs children: children
	^HTML el: 'u' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
uChildren: children
	^HTML el: 'u' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
ulAttrs: attrs children: children
	^HTML el: 'ul' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
ulChildren: children
	^HTML el: 'ul' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
varAttrs: attrs children: children
	^HTML el: 'var' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
varChildren: children
	^HTML el: 'var' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
videoAttrs: attrs children: children
	^HTML el: 'video' attrs: attrs children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
videoChildren: children
	^HTML el: 'video' children: children! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
wbr
	^HTML elc: 'wbr'! !

!HTML class methodsFor: 'dynamically added' stamp: 'RMV 10/13/2024 18:43:38'!
wbrAttrs: attrs
	^HTML elc: 'wbr' attrs: attrs! !

!MimeType class methodsFor: 'as yet unclassified' stamp: 'RMV 10/13/2024 11:47:49'!
forFileExtension: aFileExtension
	^ ExtensionToMimeDict at: aFileExtension ifAbsent: 'text/plain'! !

!MimeType class methodsFor: 'as yet unclassified' stamp: 'RMV 10/13/2024 11:48:41'!
initialize
	ExtensionToMimeDict := Dictionary newFrom: {
		'css' -> 'text/css'.
		'gif' -> 'image/gif'.
		'gz' -> 'application/gz'.
		'html' -> 'text/html'.
		'jpeg' -> 'image/jpeg'.
		'jpg' -> 'image/jpeg'.
		'js' -> 'text/javascript'.
		'json' -> 'application/json'.
		'mp4' -> 'video/mp4'.
		'ogg' -> 'audio/ogg'.
		'ogv' -> 'video/ogg'.
		'pdf' -> 'application/pdf'.
		'png' -> 'image/png'.
		'svg' -> 'image/svg+xml'.
		'ttf' -> 'font/ttf'.
		'txt' -> 'text/plain'.
		'wav' -> 'audio/wav'.
		'webm' -> 'video/webm'.
		'webp' -> 'image/webp'.
		'woff' -> 'font/woff'.
		'xml' -> 'application/xml'.
		'zip' -> 'application/zip'
	}
	! !

!WebContext methodsFor: 'private' stamp: 'RMV 9/26/2024 09:22:59'!
request: aWebRequest route: aWebRoute
	request := aWebRequest.
	route := aWebRoute.! !

!WebContext methodsFor: 'accessing' stamp: 'RMV 9/29/2024 10:15:57'!
pathParameter: aParameter
	| index tokens |
	tokens := route path findTokens: $/.
	index := tokens indexOf: ':', aParameter.
	tokens := request rawUrl findTokens: $/.
	^tokens at: index.! !

!WebContext methodsFor: 'accessing' stamp: 'RMV 9/29/2024 10:08:37'!
queryParameters
	"Answer a Dictionary containing all the query parameters."
	| dict index url |
	dict := Dictionary new.
	url := request rawUrl.
	index := url findString: '?'.
	index > 0 ifTrue: [
		| parts sub |
		sub := url copyFrom: index + 1 to: url size.
		parts := sub findTokens: '&'.
		parts do: [:part |
			| kv |
			kv := part prefixAndSuffix: $=.
			kv ifNotNil: [dict at: kv first put: kv last].
		]
	].
	^dict.! !

!WebContext methodsFor: 'accessing' stamp: 'RMV 9/26/2024 09:23:35'!
request
	^request! !

!WebContext methodsFor: 'accessing' stamp: 'RMV 9/26/2024 09:23:28'!
route
	^route! !

!WebContext class methodsFor: 'as yet unclassified' stamp: 'RMV 9/26/2024 16:56:50'!
newRequest: aWebRequest route: aWebRoute
	^WebContext new request: aWebRequest route: aWebRoute! !

!WebRoute methodsFor: 'accessing' stamp: 'RMV 10/9/2024 15:23:04'!
handler
	^handler! !

!WebRoute methodsFor: 'accessing' stamp: 'RMV 10/7/2024 11:15:18'!
key
	^method, ': ', path! !

!WebRoute methodsFor: 'accessing' stamp: 'RMV 9/23/2024 16:01:19'!
method
	^method! !

!WebRoute methodsFor: 'accessing' stamp: 'RMV 9/23/2024 16:01:28'!
path
	^path! !

!WebRoute methodsFor: 'accessing' stamp: 'RMV 9/23/2024 16:37:20'!
pattern
	^pattern! !

!WebRoute methodsFor: 'accessing' stamp: 'RMV 10/9/2024 15:28:37'!
printString
	| handlerName |
	handlerName := handler isSymbol
		ifTrue: [ handler ]
		ifFalse: [
			handler isBlock
				ifTrue: [ 'a Block' ]
				ifFalse: [ 'invalid handler' ]
		].
	^ 'WebRoute: ', method, ': ', path, ' handled by ', handlerName.! !

!WebRoute methodsFor: 'private' stamp: 'RMV 10/9/2024 15:41:53'!
handler: aSymbolOrBlock
	handler := aSymbolOrBlock! !

!WebRoute methodsFor: 'private' stamp: 'RMV 9/23/2024 16:01:50'!
method: aString
	method := aString! !

!WebRoute methodsFor: 'private' stamp: 'RMV 9/23/2024 16:01:57'!
path: aString
	path := aString! !

!WebRoute methodsFor: 'private' stamp: 'RMV 9/23/2024 16:37:51'!
pattern: aString
	pattern := aString! !

!WebRoute class methodsFor: 'instance creation' stamp: 'RMV 10/9/2024 15:36:13'!
method: methodSymbol path: pathString handler: handler
	| route |
	route := WebRoute new.
	route method: methodSymbol.
	route path: pathString.
	route handler: handler.
	route pattern: (pathString copyWithRegex: '\:\w+' matchesReplacedWith: '*').
	^route.! !

!Object methodsFor: '*WebClientPlus' stamp: 'RMV 10/12/2024 19:41:18'!
logAs: aString
	('{1} = {2}' format: {aString. self}) print! !
Dog initialize!
HTML initialize!
MimeType initialize!
