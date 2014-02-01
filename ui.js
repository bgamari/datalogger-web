function edit_text($text, $editBtn, text_change_fn) {
    var $input = $('<input>');
    var $parent = $text.parent();
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
        evt = evt ? evt : (window.event ? event : null);
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

function delete_row(uuid) {
    $row = $('#sensors').find('#s'+uuid);
    $row.fadeOut(400, function(){
        $row.remove();
    });
}

function set_status_active(uuid, is_active) {
    var $btn = $("#s"+uuid+" .activate-btn");
    var $icon = $btn.find("i");
    if (is_active) {
        $icon.attr('class', 'fa fa-check-circle-o');
        $btn.attr('title', 'Stop logging');
        $('#sensors').find('#s'+uuid).removeClass('inactive');
    } else {
        $icon.attr('class', 'fa fa-circle-o');
        $btn.attr('title', 'Start logging');
        $('#sensors').find('#s'+uuid).addClass('inactive');
    }
    $("#s"+uuid+" .activate-btn").attr('data-active', ''+is_active);
}

function set_sensor_name(uuid, name){
    var $text = $('#s'+uuid+" .sensor-name h3");
    $text.text(name);
}


function add_sensor_row(uuid, sensor_name) {
    var $row = $("<li></li>", {
        id: 's'+uuid,
        class: ['sensor']
    });
    $row.append(
        $('<span/>')
            .addClass('sensor-name')
            .append($('<h3/>').text(name))
            .append($("<button><i class='fa fa-pencil'></i></button>")
                    .addClass("edit-btn btn-s")
                    .click(function () {
                        var $sensor = $('#s'+uuid)
                        var $editBtn = $sensor.find("span.sensor-name .edit-btn");
                        var $text = $sensor.find("span.sensor-name h3");
                        edit_text($text, $editBtn, function(name) {
                            sensor_name_change(uuid, name);
                        });
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
        });

    var $del_sensor_btn = $("<button class='btn btn-sm btn-delsensor'/>")
        .attr('title', 'delete sensor from table.')
        .append($("<i class='fa fa-trash-o'></i>"))
        .click(function (event) {
            event.preventDefault();
            delete_row(uuid);
            eject_sensor(uuid);
        });

    $row.append($("<span class='sensor-activate-cell' />")
        .append($acquire_btn)
        .append($eject_btn)
        .append($del_sensor_btn)
    );

    var $plot_btn = $("<button class='btn btn-sm btn-primary plot-btn' />")
        .append($("<i class='fa fa-bar-chart-o'></i>"))
        .click(function () {
            $.ajax('/devices/'+uuid+'/samples/json', {
                success: function (data, error, xhr) {
                    filtered = [];
                    for (i in data) {
                        data[i].time *= 1000; // times expected to be in milliseconds
                        if (data[i].sensor == 1)
                            filtered.push(data[i]);
                    }
                    curve_set_data(filtered);
                    update_sparkline();
                }
            });
        });

    $row.append($("<span class='sample-count'>unknown</span>"))
        .append($plot_btn)
        .append($("<button class='btn btn-sm btn-primary download-btn'/>")
                .append($("<i class='fa fa-download'></i>"))
                .click(function () {
                    location.href = "/devices/"+uuid+"/samples/csv";
                })
               );

    $row.append(
        $("<button class='btn btn-sm btn-danger delete-btn'/>")
            .click(function (event) {
                $.ajax("/devices/"+uuid+"/erase", {
                    type: "POST",
                    success: function (data, status, xhr) {
                        $("#s"+uuid+" span.sample-count").html("0");
                    }
                });
            })
            .append($("<i class='fa fa-trash-o'></i>"))
    );

    $row.append($("<span/>", {
            class: "sparkline",
            backgroundColor: "#337744",
            width: 200,
            height: 50
        })
    );
    update_sparkline = function() {
        $.ajax("/devices/"+uuid+"/samples/json", {
            success: function (data, error, xhr) {
                filtered = [];
                for (i in data) {
                    data[i].time *= 1000; // times expected to be in milliseconds
                    if (data[i].sensor == 1)
                        filtered.push(data[i]);
                }
                var span_id = '#sensors li span.sparkline';
                sparkline_set_data(span_id, filtered);
                if (xhr.status == 202)
                    setTimeout(update_sparkline, 1000);
            }
        });
    };
    update_sparkline();

    $row.append($("<span/>")
        .append($("<span class='configuration-state'>hmm</span>"))
        .append($("<button>Configure</button>")
                .addClass('btn btn-primary btn-sm configure-btn')
                .click(function (event) {
                    var t = parseFloat($("#sample-interval").val()) * 60;
                    $.ajax("/devices/"+uuid+"/sample-period", {
                        type: "POST",
                        data: {value: t},
                        success: function (data, status, xhr) {
                            $("#s"+uuid+" .configuration-state").html("Configured");
                        }
                    });
                })
               )
    );

    update_channel_sparkline = function(channel_id) {
        $.ajax("/devices/"+uuid+"/sensors/"+channel_id+"/samples/json", {
            success: function (data, error, xhr) {
                var span_id = '#s'+uuid+' ul.channels [data-channel-id="'+channel_id+'"] .sparkline';
                sparkline_set_data(span_id, data);
            }
        });
    };

    var channels = $("<ul/>")
                   .addClass("channels");
    $.ajax("/devices/"+uuid+"/sensors", {
        success: function(data, error, xhr) {
            for (var i in data) {
                var channel = data[i];
                var ch = $("<li>")
                         .attr('data-channel-id', channel.sensor_id);
                var meta = $("<span>")
                    .addClass("meta")
                    .append("channel ")
                    .append($("<span>")
                            .addClass('channel-name')
                            .text(channel.name)
                           )
                    .append(" measured in units of ")
                    .append($("<span>")
                            .addClass('unit')
                            .text(channel.unit)
                           );

                ch.append(meta)
                  .append($("<span>")
                          .addClass('sparkline')
                         );

                channels.append(ch);
                update_channel_sparkline(channel.sensor_id);
            }
        }
    });
    $row.append(channels);

    $("#sensors").append($row);

}
