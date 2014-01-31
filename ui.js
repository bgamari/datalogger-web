function edit_text($parent, $text, $editBtn, text_change_fn) {
    var $input = $('<input>');
    $input.val($text.text());

    function submit_sensor_name() {
        var new_name = $input.val();
        $text.text(new_name);
        $input.remove();
        $text.show();
        $editBtn.show();
        text_change_fn(new_name);
    }

    $input.focusout(submit_sensor_name);
    $input.keypress(function (evt) {
        evt = (evt) ? evt : (window.event) ? event : null;
        if (evt) {
            var charCode =
                evt.charCode ? evt.charCode :
                    (evt.keyCode ? evt.keyCode :
                        (evt.which ? evt.which : 0));
            if (charCode == 13)
                submit_sensor_name();
        }
    });
    $text.hide();
    $editBtn.hide();
    $input.appendTo($parent);
}

function deactivate_row(uuid) {
    $('#sensors').find('tr#' + uuid).addClass('inactive');
}

function activate_row(uuid) {
    $('#sensors').find('tr#' + uuid).removeClass('inactive');
}

function delete_row(uuid) {
    $row = $('#sensors').find('tr#' + uuid);
    $row.fadeOut(400, function(){
        $row.remove();
    });
}

function set_status_active(uuid, is_active) {
    var $icon = $("#" + uuid + " .activate-btn i");
    if (is_active) {
        $icon.attr('class', 'fa fa-check-circle-o');
    } else {
        $icon.attr('class', 'fa fa-circle-o');
    }
    $("#" + uuid + " .activate-btn").attr('data-active', '' + is_active);
}

function set_sensor_name(uuid, name){
    var $text = $('#' + uuid + " .sensor-name");
    $text.text(name);
}


function add_sensor_row(uuid, sensor_name) {
    var $row = $("<tr></tr>", {
        id: uuid,
        class: ['sensor']
    });
    $row.append($("<td class='sensor-name-cell' />")
            .append( $('<span/>')
                .addClass("sensor-name")
                .text(name)

            )
            .append(  $("<button><i class='fa fa-pencil'></i></button>")
                .addClass("edit-btn btn-s")
                .click(function () {
                    var $editBtn = $('#' + uuid + " .edit-btn");
                    var $text = $('#' + uuid + " .sensor-name");
                    var $parent = $("#" + uuid + ' .sensor-name-cell');
                    edit_text($parent, $text, $editBtn, function(name) {sensor_name_change(uuid, name); });
                })
            )
        );

    var $acquire_btn =
        $('<button/>')
            .addClass("activate-btn")
            .addClass("btn btn-sm btn-primary")
            .attr("data-active", 'false')
            .append($('<i/>'))
            .click(function () {
                var is_acquiring = $acquire_btn.attr("data-active") == 'true';
                var start_acquiring = !is_acquiring;
                start_stop_acquire(uuid, start_acquiring);
                set_status_active(uuid, !is_acquiring);
            });

    var $eject_btn = $("<button class='btn btn-sm btn-warning'/>")
        .attr('title', 'put sensor in low power without starting acquisition.')
        .append($("<i class='fa fa-eject'></i>"))
        .click(function () {
            eject_sensor(uuid);
            deactivate_row(uuid);
        });

    var $del_sensor_btn = $("<button class='btn btn-sm btn-delsensor'/>")
        .attr('title', 'delete sensor from table.')
        .append($("<i class='fa fa-trash-o'></i>"))
        .click(function (event) {
            event.preventDefault();
            delete_row(uuid);
            eject_sensor(uuid);
        });

    $row.append($("<td class='sensor-activate-cell' />")
        .append($acquire_btn)
        .append($eject_btn)
        .append($del_sensor_btn)
    );


    var $plot_btn = $("<button class='btn btn-sm btn-primary plot-btn' />")
        .append($("<i class='fa fa-bar-chart-o'></i>"))
        .click(function () {
            $.ajax('/devices/' + uuid + '/samples/json',
                { success: function (data, error, xhr) {
                    filtered = [];
                    for (i in data) {
                        if (data[i].sensor == 1)
                            filtered.push(data[i]);
                    }
                    curve_set_data(filtered);
                    curve_set_mini_preview(uuid, $("#preview-chart-"+uuid), filtered)
                }
                }
            )
        });
    $row.append($("<td/>")
        .append($("<span class='sample-count'>unknown</span>"))
        .append($plot_btn)
        .append($("<button class='btn btn-sm btn-primary download-btn'/>")
            .append($("<i class='fa fa-download'></i>"))
            .click(function () {
                location.href = "/devices/" + uuid + "/samples/csv";
            })
        )

        .append($("<button class='btn btn-sm btn-danger delete-btn'/>")
            .click(function (event) {
                $.ajax("/devices/"+uuid+"/erase", {
                    type: "POST",
                    success: function (data, status, xhr) {
                        $("#"+uuid+" span.sample-count").text = 0;
                    }
                });
            })
            .append($("<i class='fa fa-trash-o'></i>"))
        )
    );


    $row.append($("<td></td>"))
        .append($("<div></div>",
        {
            id: "preview-chart-"+uuid,
            backgroundColor:"#337744",
            width: 200,
            height: 50
        }));

    row.append($("<td/>")
        .append($("<span class='configuration-state'>hmm</span>"))
        .append($("<button>Configure</button>")
                .addClass('btn btn-primary btn-sm configure-btn')
                .click(function (event) {
                    var t = parseFloat($("#sample-interval").val()) * 60;
                    $.ajax("/devices/"+uuid+"/sample-period", {
                        type: "POST",
                        data: {value: t},
                        success: function (data, status, xhr) {
                            $("#"+uuid+" .configuration-state").html("Configured");
                        }
                    });
                })
               )
    );

    $("#sensors").find("tbody").append($row);

}
