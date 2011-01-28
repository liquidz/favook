
var Favook = {};

Favook.constant = {
	isbnAttribute: "data-isbn"
};

Favook.getMessage = function(){
	$.getJSON("/parts/message", function(res){
		$("#message").html(res);
	});
};

Favook.getHotBooks = function(){
	$.getJSON("/hot/book", {limit: 10}, function(res){
		var hb = $("#hot_books");
		hb.html("");
		$.each(res, function(i, v){
			console.log(v);
			v.append("<p>" + v.title + "</p>");
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

$(function(){
	Favook.getMessage();

	Favook.getHotBooks();
	//Favook.getBookThumbnail("p.book", function(e, img){
	//	if(img !== null){
	//		e.append("<img src='"+img+"' />");
	//	}
	//});

});
