/* 
	XmlHttpRequest Wrapper
	Version 1.2.2
	29 Jul 2005 
	adamv.com/dev/

	"stolen" and modified by Benjamin Shropshire
	- added waitfor
	- removed caching
	- removed logging

*/

var Http = {
	ReadyState: {
		Uninitialized: 0,
		Loading: 1,
		Loaded:2,
		Interactive:3,
		Complete: 4
	},
		
	Status: {
		OK: 200,
		
		Created: 201,
		Accepted: 202,
		NoContent: 204,
		
		BadRequest: 400,
		Forbidden: 403,
		NotFound: 404,
		Gone: 410,
		
		ServerError: 500
	},
		
	Method: {Get: "GET", Post: "POST", Put: "PUT", Delete: "DELETE"},
	
	enabled: false,
	_get: null,	// Reference to the XmlHttpRequest object
	
	Init: function(){
		Http._get = Http._getXmlHttp()
		Http.enabled = (Http._get != null)
	},
	
	_getXmlHttp: function(){
	/*@cc_on @*//*@if (@_jscript_version >= 5)
		try { return new ActiveXObject("Msxml2.XMLHTTP"); } 
		catch (e) {} 
		try { return new ActiveXObject("Microsoft.XMLHTTP"); } 
		catch (e) {} 
	@end @*/
		try { return new XMLHttpRequest();}
		catch (e) {}

		return null;
	},

/*
	Params:
		url: The URL to request. Required.
		cache: Cache control. Defaults to Cache.Get.
		callback: onreadystatechange function, called when request is completed. Optional.
		method: HTTP method. Defaults to Method.Get.
*/
	get: function(params, callback_args){	
		if (!Http.enabled) throw "Http: XmlHttpRequest not available.";
		
		var url = params.url;
		if (!url) throw "Http: A URL must be specified";
				
		var method = params.method || Http.Method.Get;
		var callback = params.callback;

		// Only one request at a time, please
		if ((Http._get.readyState != Http.ReadyState.Uninitialized) && 
			(Http._get.readyState != Http.ReadyState.Complete)){
			this._get.abort();
		}
		
		Http._get.open(method, url, true);

		Http._get.onreadystatechange =  function() {
			if (Http._get.readyState != Http.ReadyState.Complete) return;
			
			if (callback_args == null) callback_args = new Array();

			var cb_params = new Array();
			cb_params.push(Http._get);
			for(var i=0;i<callback_args.length;i++)
				cb_params.push(callback_args[i]);
				
			callback.apply(null, cb_params);
		}
		
		Http._get.send(params.body || null);
	},

	waitfor: function(params){	
		if (!Http.enabled) throw "Http: XmlHttpRequest not available.";
		
		var url = params.url;
		if (!url) throw "Http: A URL must be specified";
				
		var method = params.method || Http.Method.Get;
		var callback = params.callback;

		// Only one request at a time, please
		if ((Http._get.readyState != Http.ReadyState.Uninitialized) && 
			(Http._get.readyState != Http.ReadyState.Complete)){
			this._get.abort();
		}
		
		Http._get.open(method, url, false);

		Http._get.send(params.body || null);

		return Http._get;
	},
}

Http.Init()

function getResponseProps(response, header){
	try {
		var s = response.getResponseHeader(header || 'X-Ajax-Props');
		if (s==null || s=="")
			return new Object()
		else
			return eval("o="+s)
	} catch (e) { return new Object() }
}
