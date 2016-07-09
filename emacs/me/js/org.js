// Add some bootstrap classes to the HTML exported by org-mode
window.onload = function () {
    // Add container classe to main body parts
    document.getElementById("content").className += " container";
    document.getElementById("postamble").className += " container";
    // Add table class to all tables
    var tables = document.getElementById("content").getElementsByTagName("table");
    for (var i=0; i<tables.length; ++i) {
        tables[i].className = "table";
    }
    // Add dl-horizontal to all org-mode definition lists
    var deflists = document.getElementById("content").getElementsByClassName("org-dl");
    for (var i=0; i<deflists.length; ++i) {
        deflists[i].className += " dl-horizontal";
    }
};
