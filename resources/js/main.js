
var Favook = {};

Favook.constant = {
	isbnAttribute: "data-isbn"
};


//Favook.template.book = function(obj){
//	if($.isArray(obj)){
//		return $.map(obj, function(v, i){ return Favook.template.book(v); }).join("");
//	} else {
//		"<li><a href=''></a></li>"
//	}
//};


//Favook.template.book_li = SNBinder.compile("<li><a href=''></a></li>")

Favook.getMessage = function(){ $.getJSON("/parts/message", function(res){ $("#message").html(res); }); };

Favook.applyTemplate = function(params){
	if(!params.params){ params.params = {}; }
	//if(arguments.length === 4){
	//	var url = arguments[0], params = arguments[1], templateSelector = arguments[2], targetSelector = arguments[3];
	//} else if(arguments.length === 3){
	//	var url = arguments[0], params = {}, templateSelector = arguments[1], targetSelector = arguments[2];
	//}
	//$.getJSON(url, params, function(res){
	//	//$(targetSelector).html($(templateSelector).tmpl(res));
	//	var hoge = $(templateSelector).tmpl(res);
	//	hoge.bind("click", function(){ console.log("clicked"); });
	//	$(targetSelector).html(hoge);
	//});
	$.getJSON(params.url, params.params, function(data){
		//$(targetSelector).html($(templateSelector).tmpl(res));
		var res = $(params.tmpl).tmpl(data);
//		if(params.click){ res.bind("click", params.click); }
		$(params.target).html(res);
		if($.isFunction(params.callback)){ params.callback(res); }
	});
};


//Favook.getBookThumbnail = function(selector, fn){
//	$(selector).each(function(){
//		var elem = $(this);
//		var isbn = "ISBN:" + elem.attr(Favook.constant.isbnAttribute);
//		if(isbn.length > 5){
//			$.getJSON("http://books.google.com/books?bibkeys="+isbn+"&jscmd=viewapi&callback=?",
//				function(res){
//					var thumb = res[isbn].thumbnail_url;
//					fn(elem, (thumb !== undefined) ? thumb : null);
//				}
//			);
//		}
//	});
//};

//$(document).ready(function(){ SNBinder.init({}); });

$(function(){
//	SNBinder.init({});
//
//	SNBinder.flush_all(); // dev

	Favook.getMessage();
	Favook.applyTemplate({url: "/parts/login", tmpl: "#tmplLogin", target: "#login"});
	//Favook.applyTemplate("/parts/login", "#tmplLogin", "#login");
	//Favook.applyTemplate("/hot/book", {limit: 10}, "#tmplAggregatedBook", "#hot_books ul");
	Favook.applyTemplate({url: "/hot/book", params: {limit: 10}, tmpl: "#tmplAggregatedBook", target: "#hot_books ul"});


	//Favook.applyTemplate("/hot/comment", {limit: 10}, "#tmplAggregatedBook", "#hot_comment_books ul");
	Favook.applyTemplate({url: "/hot/comment", params: {limit: 10}, tmpl: "#tmplAggregatedBook", target: "#hot_comment_books ul", callback: function(res){
		console.log("kiteru?" + res);
		res.children("a.book_link").bind("click", function(){ console.log("book link clickded"); });
		res.children("a.author_link").bind("click", function(){ console.log("author link clicked"); });

	}});


});
