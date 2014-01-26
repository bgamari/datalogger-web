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
    $("#deviceIdx-"+deviceIdx).remove();
}

function select_device(device) {
    current_device = deviceIdx;
    // setup UI elements
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
                            $addbtn = $("<button>Add Sensor</button>")
                                .click(function () {
                                    add_sensor(uuid, name)
                                });

                            $item = $("<li>" + deviceIdx+ ": "+ name + "</li>");
                            $item.append($btn);
                            $item.append($addbtn);
                            $("#devices").append($item);

                        }
                    })
                }

            }
    }});
}

function sensor_name_change(uuid, name){
    $.ajax("/devices/"+uuid+"/name", {
        type: "POST",
        data: "submit="+name
    });
}



function add_sensor(uuid, sensor_name) {

    var newRow = $('<tr><td id="sensor_name_"' + uuid + '>       \
    </td> \
        <td><button>X</button></td> \
        <td>2014 <button id="sensor' + uuid + '_deleteall">Del</button></td> \
        <td>2 min</td> \
        <td>Ok <button id="sensor' + uuid + '_confupl">Configure</button></td></tr> \
    ');
    var row = $("<tr></tr>", {id: uuid});
    row.append($("<td class='sensor-name-cell'></td>"));
    row.append($("<td class='sensor-name-edit-cell'></td>"));
    row.append($("<td><button>X</button></td>"));
    row.append($("<td>20134 <button class='delete-btn'>Del</button></td>"));
    row.append($("<td>2 min</td>"));
    row.append($("<td>Ok <button class='configure-btn'>Configure</button></td>"));

    $("#sensors").find("tbody").append(row);
    add_sensor_name(uuid, sensor_name, function(name) { sensor_name_change(uuid, name);});
}
function add_new_sensor() {
    var numRows = $("#sensors").find("tr").length;
    add_sensor(numRows + 1);
}

