'From Cuis7.1 [latest update: #6792] on 22 October 2024 at 7:36:18 pm'!
'Description '!
!provides: 'WebClientPlus' 1 35!
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

!classDefinition: #HtmxServer category: #WebClientPlus!
WebServerPlus subclass: #HtmxServer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'WebClientPlus'!
!classDefinition: 'HtmxServer class' category: #WebClientPlus!
HtmxServer class
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

!WebServerPlus methodsFor: 'initialization' stamp: 'RMV 10/22/2024 19:28:42'!
initialize
	super initialize.
	
	dynamicRoutes := OrderedCollection new.
	options := Dictionary new.
	staticFilePath := ''.
	staticRoutes := Dictionary new.
	
	"This handles all requests."
	action := [:request |
		| route |
		'got request for ', request method, ': ', request url :: print.
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

!DogWebServer methodsFor: 'initialization' stamp: 'RMV 10/22/2024 19:35:53'!
initialize
	| dog1 dog2 |
		
	super initialize.
	self staticFilePath: 'public'.
	
	"Create some initial dogs."
	dog1 := Dog name: 'Comet' breed: 'Whippet'.
	dog2 := Dog name: 'Oscar' breed: 'German Shorthaired Pointer'.
	dogDict := Dictionary newFrom: {
		dog1 id -> dog1.
		dog2 id -> dog2
	}.
	
	"Register routes."
	self method: #GET path: '/hello' handler: [ :context |
		context text: 'Hello, World!!'
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

!DogWebServer methodsFor: 'private' stamp: 'RMV 10/17/2024 12:45:06'!
deleteAllDogs: aWebContext
	"This should only be used by the setUp method in DogWebServerTests."	

	dogDict := Dictionary new.

	"Return an empty response."
	^ aWebContext text: ''
! !

!DogWebServer methodsFor: 'message handling' stamp: 'RMV 10/17/2024 13:12:43'!
createDog: aWebContext
 	| dog |
	
	"Get Dog object from request body. An id will be assigned."
	dog := Dog fromJson: aWebContext request stream.
	
	"Save the new dog."
	dogDict at: dog id put: dog.
	
	"Return the new dog as JSON."
	^ aWebContext json: (Json render: dog) status: 201. "Created"

! !

!DogWebServer methodsFor: 'message handling' stamp: 'RMV 10/17/2024 12:54:27'!
deleteDog: aWebContext
	| id req |
	
	req := aWebContext request.
	id := aWebContext pathParameter: #id.
	id := [id asNumber] on: Error do: [ :e | ^ aWebContext clientError: e messageText ].
	
	"Delete the dog, verifying that a dog with the given id exists."
	dogDict removeKey: id ifAbsent: [ ^ aWebContext notFound ].

	"Return an empty response."
	^ aWebContext text: ''
! !

!DogWebServer methodsFor: 'message handling' stamp: 'RMV 10/17/2024 12:55:21'!
getDogAsJson: aWebContext
	| dog id |
	
	id := aWebContext pathParameter: #id.
	id := [id asNumber] on: Error do: [ :e | ^ aWebContext clientError: e messageText ].
	
	dog := dogDict at: id ifAbsent: nil.
	dog
		ifNil: [ aWebContext notFound ]
		ifNotNil: [ aWebContext json: (Json render: dog) ]! !

!DogWebServer methodsFor: 'message handling' stamp: 'RMV 10/17/2024 12:59:49'!
getDogs: aWebContext	
	| queryParams |
	
	"This just demonstrates that we can get query parameters."
	queryParams := aWebContext queryParameters.
	queryParams logAs: 'queryParams'.
	
	"Only return HTML if requested in the Accept header.
	Otherwise default to returning JSON."
	aWebContext requestHeader: 'Accept' :: includesSubString: 'text/html' ::
		ifTrue: [^self getDogsAsHtml: aWebContext]
		ifFalse: [^self getDogsAsJson: aWebContext]! !

!DogWebServer methodsFor: 'message handling' stamp: 'RMV 10/18/2024 19:42:06'!
getDogsAsHtml: aWebContext
	| dogs spec tableRows |
	
	"Create array of dogs sorted on their names."
	dogs := dogDict values sort: [:a :b | a name < b name].
	
	"Create array of arrays that describe tables rows for the dogs."
	tableRows := dogs collect: [ :dog |
		{ #tr. { #td. dog name}. { #td. dog breed } }.
	].

	spec := Dictionary newFrom: {
		#title->'My Dogs'.
		#stylesheets->#('dog.css').
		#body->{
			{#h1. 'My Dogs'}.
			{ #table.
				{ #tr. { #th. 'Name' }. { #th. 'Breed' } }
			}, tableRows
		}
	}.

	aWebContext document: spec.
! !

!DogWebServer methodsFor: 'message handling' stamp: 'RMV 10/17/2024 12:05:58'!
getDogsAsJson: aWebContext
	aWebContext json: (Json render: dogDict asArray)
	
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

!DogWebServer methodsFor: 'message handling' stamp: 'RMV 10/17/2024 13:05:12'!
patchDog: aWebContext
	| dog id jsonObject |
	
	id := aWebContext pathParameter: #id.
	id := [id asNumber] on: Error do: [ :e | ^ aWebContext clientError: e messageText ].

	"Verify that a dog with the given id exists."
	dog := dogDict at: id ifAbsent: [ ^ aWebContext notFound ].

	"Get JSON from request body."
	jsonObject := Json readFrom: aWebContext request stream.

	"Patch the dog object."
	jsonObject associationsDo: [:assoc |
		| selector |
		selector := assoc key, ':' :: asSymbol.
		dog perform: selector with: assoc value.
	].
	dogDict at: id put: dog.
		
	"Return the updated dog as JSON."
	^ aWebContext json: (Json render: dog).
! !

!DogWebServer methodsFor: 'message handling' stamp: 'RMV 10/17/2024 13:04:57'!
updateDog: aWebContext
	| dog id |
	
	id := aWebContext pathParameter: #id.
	id := [id asNumber] on: Error do: [ :e | ^ aWebContext clientError: e messageText ].

	"Verify that a dog with the given id exists."
	dogDict at: id ifAbsent: [^ aWebContext notFound ].

	"Get Dog object from request body."
	dog := Dog fromJson: aWebContext request stream.
	
	"Update its id to match id parameter."
	dog id: id.
	
	"Save the change."
	dogDict at: dog id put: dog.
	
	"Return the updated dog as JSON."
	^ aWebContext json: (Json render: dog).! !

!HtmxServer methodsFor: 'initialization' stamp: 'RMV 10/22/2024 19:22:42'!
index: aWebContext
	| spec |

	spec := Dictionary newFrom: {
		#title->'My htmx Demo'.
		#stylesheets->#('demo.css').
		#scripts->#('https://unpkg.com/htmx.org@2.0.3').
		#body->{
			{#h1. 'My htmx Demo'}.
			{#button. 'hx-get'->'/version'. 'hx-target'->'#version'. 'Get Version'}.
			{#div. #id->'version'}
		}
	}.
	aWebContext document: spec.! !

!HtmxServer methodsFor: 'initialization' stamp: 'RMV 10/17/2024 16:21:41'!
initialize		
	super initialize.
	self staticFilePath: 'public'.	
	
	self method: #GET path: '/version' handler: [ :context |
		context text: (SystemVersion current versionString)
	].
	
	self method: #GET path: '/' handler: #index:.! !

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

!WebContext methodsFor: 'accessing' stamp: 'RMV 10/17/2024 12:51:51'!
clientError: aString
	self request send400Response: aString
! !

!WebContext methodsFor: 'accessing' stamp: 'RMV 10/18/2024 19:12:51'!
document: specDict
	| body bodyContent head scripts stylesheets title |
	
	head := OrderedCollection newFrom: #(#head).
	
	title := specDict at: #title ifAbsent: nil.
	title ifNotNil: [ head add: 				{#title. 'My htmx Demo'} ].
	
	stylesheets := specDict at: #stylesheets ifAbsent: #().
	stylesheets do: [ :link | head add: {#link. #rel->#stylesheet. #href->link} ].
	
	scripts := specDict at: #scripts ifAbsent: #().
	scripts do: [ :script | head add: {#script. #src->script} ].

	bodyContent := specDict at: #body ifAbsent: #().
	body := {#body}, bodyContent.

	self html: (WebContext generateHtmlString: { #html. #lang->#en. head. body. }).! !

!WebContext methodsFor: 'accessing' stamp: 'RMV 10/17/2024 10:27:22'!
html: aString
	self request send200Response: aString contentType: 'text/html'

! !

!WebContext methodsFor: 'accessing' stamp: 'RMV 10/17/2024 10:29:03'!
json: aString
	self request send200Response: aString contentType: 'application/json'

! !

!WebContext methodsFor: 'accessing' stamp: 'RMV 10/17/2024 13:11:26'!
json: aString status: anHttpStatusCode
	| req res |
	
	req := self request.
	res := WebResponse protocol: req protocol code: anHttpStatusCode.
	res headerAt: 'Content-Type' put: 'application/json'.
	req sendResponse: res content: aString
! !

!WebContext methodsFor: 'accessing' stamp: 'RMV 10/17/2024 12:49:55'!
notFound
	self request send404Response
! !

!WebContext methodsFor: 'accessing' stamp: 'RMV 9/29/2024 10:15:57'!
pathParameter: aParameter
	| index tokens |
	tokens := route path findTokens: $/.
	index := tokens indexOf: ':', aParameter.
	tokens := request rawUrl findTokens: $/.
	^tokens at: index.! !

!WebContext methodsFor: 'accessing' stamp: 'RMV 10/17/2024 12:58:38'!
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

!WebContext methodsFor: 'accessing' stamp: 'RMV 10/17/2024 12:59:04'!
requestHeader: aString
	"Answer the value of a given request header."
	^ self request headerAt: aString
! !

!WebContext methodsFor: 'accessing' stamp: 'RMV 9/26/2024 09:23:28'!
route
	^route! !

!WebContext methodsFor: 'accessing' stamp: 'RMV 10/17/2024 12:51:58'!
serverError: aString
	self request send500Response: aString
! !

!WebContext methodsFor: 'accessing' stamp: 'RMV 10/17/2024 10:26:30'!
text: aString
	self request send200Response: aString
! !

!WebContext class methodsFor: 'as yet unclassified' stamp: 'RMV 10/18/2024 19:13:35'!
generateHtmlString: specColl
	"Generates a String of HTML from an Array."
	| elementName rest stream |
	
	specColl isKindOf: SequenceableCollection :: ifFalse: [
		Error signal: 'WebContent class method html: requires SequenceableCollection argument'
	].

	elementName := specColl first.
	elementName isKindOf: CharacterSequence :: ifFalse: [
		Error signal: 'WebContent class method html: specColl first element must be CharacterSequence but was ', elementName class name
	].

	stream := WriteStream on: (String new: 100).
	stream nextPut: $<.
	stream nextPutAll: elementName.
	
	rest := specColl allButFirst.
	rest do: [:item |
		item class = Association ifTrue: [
			| key |
			key := item key.
			key isKindOf: CharacterSequence :: ifFalse: [
				Error signal: 'WebContent class method html: Association must have CharacterSequence key'
			].
			stream nextPut: Character space.
			stream nextPutAll: key.
			stream nextPut: $=.
			stream nextPut: $".
			stream nextPutAll: item value asString.
			stream nextPut: $".
		]
	].

	stream nextPut: $>.

	rest do: [:item |
		item isKindOf: CharacterSequence ::
			ifTrue: [
				item class = Association ifFalse: [
					stream nextPutAll: item asString
				]
			]
			ifFalse: [
				item isKindOf: SequenceableCollection ::
					ifTrue: [ stream nextPutAll: (self html: item) ]
			]
	].

	self selfClosingElements includes: elementName :: ifFalse: [
		stream nextPut: $<.
		stream nextPut: $/.
		stream nextPutAll: elementName.
		stream nextPut: $>.
	].

	^ stream contents.



! !

!WebContext class methodsFor: 'as yet unclassified' stamp: 'RMV 10/18/2024 19:13:03'!
generateHtmlStringh: specColl
	"Generates a String of HTML from an Array."
	| elementName rest stream |
	
	specColl isKindOf: SequenceableCollection :: ifFalse: [
		Error signal: 'WebContent class method html: requires SequenceableCollection argument'
	].

	elementName := specColl first.
	elementName isKindOf: CharacterSequence :: ifFalse: [
		Error signal: 'WebContent class method html: specColl first element must be CharacterSequence but was ', elementName class name
	].

	stream := WriteStream on: (String new: 100).
	stream nextPut: $<.
	stream nextPutAll: elementName.
	
	rest := specColl allButFirst.
	rest do: [:item |
		item class = Association ifTrue: [
			| key |
			key := item key.
			key isKindOf: CharacterSequence :: ifFalse: [
				Error signal: 'WebContent class method html: Association must have CharacterSequence key'
			].
			stream nextPut: Character space.
			stream nextPutAll: key.
			stream nextPut: $=.
			stream nextPut: $".
			stream nextPutAll: item value asString.
			stream nextPut: $".
		]
	].

	stream nextPut: $>.

	rest do: [:item |
		item isKindOf: CharacterSequence ::
			ifTrue: [
				item class = Association ifFalse: [
					stream nextPutAll: item asString
				]
			]
			ifFalse: [
				item isKindOf: SequenceableCollection ::
					ifTrue: [ stream nextPutAll: (self html: item) ]
			]
	].

	self selfClosingElements includes: elementName :: ifFalse: [
		stream nextPut: $<.
		stream nextPut: $/.
		stream nextPutAll: elementName.
		stream nextPut: $>.
	].

	^ stream contents.



! !

!WebContext class methodsFor: 'as yet unclassified' stamp: 'RMV 10/18/2024 19:06:54'!
html: specColl
	"Generates a String of HTML from an Array."
	| elementName rest stream |
	
	specColl isKindOf: SequenceableCollection :: ifFalse: [
		Error signal: 'WebContent class method html: requires SequenceableCollection argument'
	].

	elementName := specColl first.
	elementName isKindOf: CharacterSequence :: ifFalse: [
		Error signal: 'WebContent class method html: specColl first element must be CharacterSequence but was ', elementName class name
	].

	stream := WriteStream on: (String new: 100).
	stream nextPut: $<.
	stream nextPutAll: elementName.
	
	rest := specColl allButFirst.
	rest do: [:item |
		item class = Association ifTrue: [
			| key |
			key := item key.
			key isKindOf: CharacterSequence :: ifFalse: [
				Error signal: 'WebContent class method html: Association must have CharacterSequence key'
			].
			stream nextPut: Character space.
			stream nextPutAll: key.
			stream nextPut: $=.
			stream nextPut: $".
			stream nextPutAll: item value asString.
			stream nextPut: $".
		]
	].

	stream nextPut: $>.

	rest do: [:item |
		item isKindOf: CharacterSequence ::
			ifTrue: [
				item class = Association ifFalse: [
					stream nextPutAll: item asString
				]
			]
			ifFalse: [
				item isKindOf: SequenceableCollection ::
					ifTrue: [ stream nextPutAll: (self html: item) ]
			]
	].

	self selfClosingElements includes: elementName :: ifFalse: [
		stream nextPut: $<.
		stream nextPut: $/.
		stream nextPutAll: elementName.
		stream nextPut: $>.
	].

	^ stream contents.



! !

!WebContext class methodsFor: 'as yet unclassified' stamp: 'RMV 9/26/2024 16:56:50'!
newRequest: aWebRequest route: aWebRoute
	^WebContext new request: aWebRequest route: aWebRoute! !

!WebContext class methodsFor: 'as yet unclassified' stamp: 'RMV 10/17/2024 09:15:40'!
selfClosingElements
	^ Set newFrom: #(
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
	).! !

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
MimeType initialize!
