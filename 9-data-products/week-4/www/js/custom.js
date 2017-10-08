// A $( document ).ready() block.
$(document).ready(function () {
    console.log("app started");
    showDiv(1);
});

function showDiv(divId) {
    if (divId === 1) {
        $("#div1").show();
        $("#div2").hide();
    } else {
        $("#div1").hide();
        $("#div2").show();
    }
    
}