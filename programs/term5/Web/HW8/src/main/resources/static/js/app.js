window.notify = function (message) {
    $.notify(message, {
        position: "right bottom",
        className: "success"
    });
}

window.notifyError = function (message) {
    $.notify(message, {
        position: "right bottom",
        className: "error"
    });
}