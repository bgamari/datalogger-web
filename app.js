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

function eject_device(device) {
    $("#deviceIdx-"+deviceIdx).remove();
}

function refresh_devices() {
    $.ajax("/devices", {
        type: "POST",

        success: function(data, status, xhr) {
            $("#devices").empty();
            for (deviceIdx in data) {
                $btn = $("<button type='button' class='btn btn-sm btn-warning'><i class='fa fa-eject'></i></button>")
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
    var row = $("<tr></tr>", {
        id: uuid,
        class: 'sensor',
    });
    row.append($("<td class='sensor-name-cell' />"));
    row.append($("<td class='sensor-activate-cell' />"));
    row.append($("<td/>")
               .append($("<span class='sample-count'>unknown</span>"))
               .append($("<button class='btn btn-sm btn-primary download-btn'/>")
                       .append($("<i class='fa fa-download'></i>"))
                       .click(function() {
                           location.href = "/devices/" + uuid + "/samples/csv";
                       })
                      )

               .append($("<button class='btn btn-sm btn-danger delete-btn'/>")
                       .append($("<i class='fa fa-trash-o'></i>"))
                       )
              );
    row.append($("<td>2 min</td>"));
    row.append($("<td/>")
               .append($("Ok"))
               .append($("<button class='btn btn-primary btn-sm configure-btn'>Configure</button>"))
               );

    $("#sensors tbody").append(row);
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
