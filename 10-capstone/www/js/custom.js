var colnames = [];
// A $( document ).ready() block.
$(document).ready(function() {
    console.log("app started");

    // Blue skin
    $('.s-skin-1').click(function() {
        $("body").removeClass("skin-2");
        $("body").removeClass("skin-3");
        $("body").addClass("skin-1");
    });

    showDiv(1);
});

function showDiv(divId) {
    if (divId === 1) {
        $("#div1").show();
        $("#div2").hide();
        $("#div3").hide();
    } else if (divId === 2) {
        $("#div1").hide();
        $("#div2").show();
        $("#div3").hide();
    } else {
        $("#div1").hide();
        $("#div2").hide();
        $("#div3").show();
    }
}