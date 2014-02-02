function sensor_name_change(uuid, name){
    $.ajax("/devices/"+uuid+"/name", {
        type: "POST",
        data: {value: name}
    });
}

function eject_sensor(uuid){
    $.ajax("/devices/"+uuid+"/eject", {
        type: "POST",
        data: {}
    });
}

function start_stop_acquire(uuid, acquire) {
    $.ajax("/devices/"+uuid+"/acquiring", {
        type: "POST",
        data: {value: acquire},
        success: function(data, status, xhr) {
            $.ajax("/devices/"+uuid+"/acquire-on-boot", {
                type: "POST",
                data: {value: acquire},
            });
            if(start_acquiring) {
                eject_sensor(uuid);
            }
        }
    });
}

function get_status(uuid){
    $.ajax("/devices/"+uuid+"/name", {
        type: "GET",
        success: function(data, status, xhr) {
            var name = data.value;
            set_sensor_name(uuid, name);
        }
    });
    $.ajax("/devices/"+uuid+"/acquiring", {
        type: "GET",
        success: function(data, status, xhr) {
            var is_active = data.value;
            set_status_active(uuid, is_active);
        }
    });
    $.ajax("/devices/"+uuid+"/sample_count", {
        type: "GET",
        success: function(data, status, xhr) {
            $("#s" + uuid + ' .sample-count .value').html(data.value);
        }
    });
}

function add_sensor(uuid, sensor_name) {
    add_sensor_row(uuid, sensor_name);
    get_status(uuid);
}

function refresh_devices() {
    $.ajax("/devices", {
        type: "POST",

        success: function(data, status, xhr) {
            for (var idx in data) {
                var uuid = data[idx].toString();
                if ($("#sensors").find("#s" + uuid).length == 0) {
                    $.ajax("/devices/" + uuid + "/name", {
                        type: "GET",
                        success: function (namedata, status2, xhr2) {
                            name = namedata['value'];
                            add_sensor(uuid, name);
                        }
                    })

                }  else {
                    get_status(uuid);
                }
            }
    }});
}

function add_new_sensor() {
    var numRows = $("#sensors").find("tr").length;
    add_sensor(numRows + 1);
}

$(window).load(function() {
    refresh_devices();
});
