var current_device = null;

function set_acquiring(acquiring) {
    if (acquiring) {
        $("#start").addClass("disabled");
        $("#stop").removeClass("disabled");
    } else {
        $("#start").removeClass("disabled");
        $("#stop").addClass("disabled");
    }
}

function start_acquire() {
    $.ajax("/dev/start", { type: "PUT" });
    set_acquiring(true);
}

function eject_device(device) {
    $("#device-"+device).remove();
}

function select_device(device) {
    current_device = device;
    // setup UI elements
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
