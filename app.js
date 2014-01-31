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

function start_stop_acquire(uuid, start_acquiring) {
    $.ajax("/devices/"+uuid+"/acquiring", {
        type: "POST",
        data: {value: start_acquiring},
        success: function(data, status, xhr) {
            if(start_acquiring){
                deactivate_row(uuid);
                eject_sensor(uuid);
            }
        }
    });
}

function get_status(uuid){
    $.ajax("/devices/"+uuid+"/acquiring", {
        type: "GET",
        success: function(data, status, xhr) {
            var is_active = data;
            set_status_active(uuid, is_active);
        }
    });
    $.ajax("/devices/"+uuid+"/sample_count", {
        type: "GET",
        success: function(data, status, xhr) {
            $("#" + uuid + ' .sample-count').text(data);
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
            for (deviceIdx in data) {
                var uuid = data[deviceIdx].toString();
                if ($("#sensors").find("#" + uuid).length == 0) {
                    $.ajax("/devices/" + data[deviceIdx] + "/name", {
                        type: "GET",
                        success: function (name, status2, xhr2) {
                            add_sensor(uuid,name);
                        }
                    })
                }  else {
                    activate_row(uuid);
                }
            }
    }});
}


function add_new_sensor() {
    var numRows = $("#sensors").find("tr").length;
    add_sensor(numRows + 1);
}
