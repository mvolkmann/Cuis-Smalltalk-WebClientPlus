'From Cuis7.1 [latest update: #6757] on 12 October 2024 at 1:54:40 pm'!
'Description '!
!provides: 'WebClientPlus' 1 3!
!requires: 'WebClient' 1 37 nil!
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
	instanceVariableNames: 'action dynamicRoutes methods options staticRoutes'
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


!Dog commentStamp: 'RMV 6/22/2024 20:06:39' prior: 0!
Instances of this class describe a dog.!

!DogWebServerTests methodsFor: 'testing' stamp: 'RMV 10/11/2024 10:55:22'!
setUp
	| dog url |
	
	url := self baseUrl.

	"Delete all the dogs."
	WebClientPlus method: #DELETE url: url.

	"Create initial set of dogs"
	dog := Dog name: 'Comet' breed: 'Whippet'.
	WebClientPlus method: #POST url: url content: dog asJsonString.
	dog := Dog name: 'Oscar' breed: 'German Shorthaired Pointer'.
	WebClientPlus method: #POST url: url content: dog asJsonString.! !

!DogWebServerTests methodsFor: 'testing' stamp: 'RMV 10/9/2024 18:58:16'!
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
	
	"Attempt to get the deleted dog."
	res := WebClientPlus method: #GET url: url.
	self assert: res code equals: 404.
	
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

!DogWebServerTests methodsFor: 'testing' stamp: 'RMV 10/9/2024 18:46:20'!
testHeadDogs
	| contentLength res url |

	url := self baseUrl.

	"Send a GET request in order to compute the expected content length."
	res := WebClientPlus method: #GET url: url.	
	self assert: res code equals: 200.
	contentLength := res content size.

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

!DogWebServerTests methodsFor: 'testing' stamp: 'RMV 10/9/2024 18:58:32'!
testPatch
	| baseUrl dog dogs json jsonArray name res url |

	"Get any one of the current dogs."
	baseUrl := self baseUrl.
	res := WebClientPlus method: #GET url: baseUrl.
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
	self assert: (json at: #name) equals: name.
	self assert: (json at: #breed) equals: dog breed.
	
	"Get the modified dog."
	res := WebClientPlus method: #GET url: url.
	self assert: res code equals: 200.
	json := Json readFrom: res stream.
	self assert: (json at: #name) equals: name.
	self assert: (json at: #breed) equals: dog breed.
! !

!DogWebServerTests methodsFor: 'testing' stamp: 'RMV 10/9/2024 18:40:38'!
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
	
	"Get the dog just created."
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

!DogWebServerTests methodsFor: 'as yet unclassified' stamp: 'RMV 10/9/2024 18:38:31'!
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

!WebServerPlus methodsFor: 'controlling' stamp: 'RMV 10/8/2024 08:33:19'!
start
	self listenOn: 3000! !

!WebServerPlus methodsFor: 'controlling' stamp: 'RMV 10/8/2024 08:33:24'!
stop
	self destroy! !

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

!WebServerPlus methodsFor: 'initialization' stamp: 'RMV 10/9/2024 15:53:19'!
initialize
	super initialize.
	
	dynamicRoutes := OrderedCollection new.
	options := Dictionary new.
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

!WebServerPlus methodsFor: 'routing' stamp: 'RMV 10/9/2024 15:48:48'!
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

	"Handle 405 method not allowed"
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

!DogWebServer methodsFor: 'message handling' stamp: 'RMV 10/9/2024 18:57:55'!
createDog: aWebContext
 	| dog req res |
	
	req := aWebContext request.
	
	"Get Dog object from request body. An id will be assigned."
	dog := Dog fromJson: req stream.
	
	"Save the new dog."
	dogDict at: dog id put: dog.
	
	"Return the new dog as JSON."
	res := WebResponse protocol: req protocol code: 201. "Created"
	res headerAt: 'Content-Type' put: 'application/json; charset=utf-8'.
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

!DogWebServer methodsFor: 'message handling' stamp: 'RMV 10/9/2024 13:44:33'!
getDogAsJson: aWebContext
	| dog id req |
	
	req := aWebContext request.
	id := aWebContext pathParameter: #id.
	id := [id asNumber] on: Error do: [ :e | ^ req send400Response: e messageText].
	
	dog := dogDict at: id ifAbsent: nil.
	dog
		ifNil: [ req send404Response ]
		ifNotNil: [
			req send200Response: (Json render: dog) contentType: 'application/json; charset=utf-8'
		].	
! !

!DogWebServer methodsFor: 'message handling' stamp: 'RMV 10/9/2024 18:29:19'!
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

!DogWebServer methodsFor: 'message handling' stamp: 'RMV 10/8/2024 12:44:42'!
patchDog: aWebContext
	| dog id jsonObject req |
	
	req := aWebContext request.
	id := aWebContext pathParameter: #id.
	id := [id asNumber] on: Error do: [ :e | ^ req send400Response: e messageText].

	"Verify that a dog with the given id exists."
	dog := dogDict at: id ifAbsent: [^ req send404Response].

	"Get JSON from request body."
	jsonObject := Json readFrom: req stream.
	jsonObject logAs: 'jsonObject'.

	"Patch the dog object."
	jsonObject associationsDo: [:assoc |
		| selector |
		selector := assoc key, ':' :: asSymbol.
		dog perform: selector with: assoc value.
	].
	dogDict at: id put: dog.
		
	"Return the updated dog as JSON."
	^ req send200Response: (Json render: dog) contentType: 'application/json; charset=utf-8'
! !

!DogWebServer methodsFor: 'message handling' stamp: 'RMV 10/9/2024 18:58:07'!
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
	^ req send200Response: (Json render: dog) contentType: 'application/json; charset=utf-8'
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
Dog initialize!
