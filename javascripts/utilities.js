window.onload = function() {
    // 
    var clip = new ZeroClipboard(document.getElementById("btn"));
    clip.on("ready", function() {
    });
};

$(function(){
    $('a[href=#]').click(function(e){
        e.preventDefault();
    })
});
