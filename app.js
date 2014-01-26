function sensor_name_change(uuid, name){
    $.ajax("/devices/"+uuid+"/name", {
        type: "PUT",
        data: name
    });
}

function start_stop_acquire(uuid, is_active) {
    $.ajax("/devices/"+uuid+"/acquiring", { type: "POST" , data: {value: is_active}});
}

function get_status(uuid){
    $.ajax("/devices/"+uuid+"/acquiring", {
        type: "GET",
        success: function(data, status, xhr) {
            var is_active = data;
            set_status_active_uuid(uuid, is_active);
        }
    } );
}

function eject_device(device) {
    $("#deviceIdx-"+deviceIdx).remove();
}

function refresh_devices() {
    $.ajax("/devices", {
        type: "POST",

        success: function(data, status, xhr) {
            $("#devices").empty();
            for (deviceIdx in data) {
                $btn = $("<button type='button' class='btn btn-xs btn-warning'><i class='fa fa-eject'></i></button>")
                $btn.on('click', eject_device(deviceIdx));

                var uuid = data[deviceIdx].toString();

                if ($("#sensors").find("#" + uuid).length == 0) {
                    // new sensor!
                    // get name, add new row in table
                    $.ajax("/devices/" + data[deviceIdx] + "/name", {
                        type: "GET",
                        success: function (name, status2, xhr2) {
                            $item = $("<li>" + deviceIdx+ ": "+ name + "</li>");
                            $item.append($btn);

                            $addbtn = $("<button>Add Sensor</button>")
                                .click(function () {
                                    $item.remove();
                                    add_sensor(uuid, name);
                                });
                            $item.append($addbtn);
                            $("#devices").append($item);

                        }
                    })
                }

            }
    }});
}



function add_sensor(uuid, sensor_name) {
    var row = $("<tr></tr>", {id: uuid});
    row.append($("<td class='sensor-name-cell'></td>"));
    row.append($("<td class='sensor-name-edit-cell'></td>"));

    row.append($("<td class='sensor-activate-cell'></td>"));
    row.append($("<td>20134 <button class='btn btn-xs btn-danger delete-btn'><i class='fa fa-trash-o'></i></button></td>"));
    row.append($("<td>2 min</td>"));
    row.append($("<td>Ok <button class='configure-btn'>Configure</button></td>"));

    $("#sensors").find("tbody").append(row);
    add_sensor_name(uuid, sensor_name, function(name) {
        sensor_name_change(uuid, name);
    });

    add_activate_btn(uuid, false, function(is_active) {
        start_stop_acquire(uuid, is_active);
    });

    get_status(uuid);
}

function add_new_sensor() {
    var numRows = $("#sensors").find("tr").length;
    add_sensor(numRows + 1);
}

