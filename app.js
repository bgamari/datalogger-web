function start_acquire() {
    $.ajax("/dev/start", { type: "PUT" });
}

function eject_device(device) {
    $("#device-"+device).remove();
}

function refresh_devices() {
    $.ajax("/devices", {
        type: "POST",
        success: function(data, status, xhr) {
            $("#devices").empty();
            for (device in data) {
                btn = $("<button type='button' class='btn btn-xs btn-warning'><i class='fa fa-eject'></i></button>")
                btn.on('click', eject_device(device));
                item = $("<li>"+device+"</li>");
                item.append(btn);
                $("#devices").append(item);
            }
    }});
}
