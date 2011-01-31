
var Favook = {};

Favook.constant = {
	isbnAttribute: "data-isbn"
};

Favook.template = {};
Favook.template.base = function(fn){
	var self = function(obj){
		if($.isArray(obj)){
			return $.map(obj, function(v){ return self(v); }).join("");
		} else {
			return fn(obj);
		}
	};
	return self;
}
//Favook.template.book = function(obj){
//	if($.isArray(obj)){
//		return $.map(obj, function(v, i){ return Favook.template.book(v); }).join("");
//	} else {
//		"<li><a href=''></a></li>"
//	}
//};


//Favook.template.book_li = SNBinder.compile("<li><a href=''></a></li>")


Favook.template.book = Favook.template.base(function(act){
		SNBinder.bind("<li><a href=''></a></li>")
	return "<li>"+act.book.title+"</li>";
});

Favook.href = SNBinder.compile("<a href='$(.href)' title='$(.title)'>$(.caption)</a>")

Favook.getHotBooks = function(){
	$.getJSON("/hot/book", {limit: 10}, function(res){
		console.log("kiteru = " + res);
		var hb = $("#hot_books");
		hb.html("");
		$.each(res, function(i, v){
			console.log(v);
			hb.append("<p>" + v.book.title + "</p>");
		});
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
	SNBinder.init({});

	SNBinder.flush_all(); // dev

	// load message
	$("#message").load("/parts/message");


	//console.log(SNBinder.bind_rowset("<s>$(.name)</s>", [{name:"neko"}, {name:"inu"}]));

	//console.log(Favook.template.book({book: {title: "hello"}}));

	$.getJSON("/hot/book", function(books){
		$("#hot_books ul").html(Favook.template.book(books));
	});

	//SNBinder.get_named_sections("/static/template.htm", {}, function(templates){
	//	SNBinder.get("/hot/book", {}, true, function(books){
	//		$("#hot_books ul").html(SNBinder.bind_rowset(templates.book, books))
	//	});
	//});

	//Favook.getHotBooks();
	//Favook.getBookThumbnail("p.book", function(e, img){
	//	if(img !== null){
	//		e.append("<img src='"+img+"' />");
	//	}
	//});

});
