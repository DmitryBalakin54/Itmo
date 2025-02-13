window.notify = function (message) {
    $.notify(message, {
        position: "right bottom",
        className: "success"
    });
}


function ajax(data, successFun) {
    $.ajax({
        type: "POST",
        url: "",
        dataType: "json",
        data: data,
        success: function(response) {
            if (response.redirect) {
                location.href = response.redirect;
            } else {
                successFun(response);
            }
        }
    });
}