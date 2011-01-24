
var Favook = {};

Favook.constant = {
	isbnAttribute: "data-isbn"
};

Favook.getBookThumbnail = function(selector, fn){
	$(selector).each(function(){
		var elem = $(this);
		var isbn = "ISBN:" + elem.attr(Favook.constant.isbnAttribute);
		if(isbn.length > 5){
			$.getJSON("http://books.google.com/books?bibkeys="+isbn+"&jscmd=viewapi&callback=?",
				function(res){
					var thumb = res[isbn].thumbnail_url;
					fn(elem, (thumb !== undefined) ? thumb : null);
				}
			);
		}
	});
};

$(function(){
	Favook.getBookThumbnail("p.book", function(e, img){
		if(img !== null){
			e.append("<img src='"+img+"' />");
		}
	});
});
