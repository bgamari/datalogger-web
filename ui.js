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
    $input.prependTo($parent);
}

function delete_row(uuid) {
    $row = $('#sensors').find('#s'+uuid);
    $row.fadeOut(400, function(){
        $row.remove();
    });
}

function set_status_active(uuid, is_active) {
    var $btn = $("#s"+uuid+" .activate-btn");
    if (is_active) {
        $("#s"+uuid).attr('data-acquiring', 'running');
        $('#s'+uuid+' ul li.acquire-state span .value').text('logging');
        $btn.html("<i class='fa fa-check-circle-o'></i> Stop logging");
        $btn.attr('title', 'Stop logging');
        $('#sensors').find('#s'+uuid).removeClass('inactive');
    } else {
        $("#s"+uuid).attr('data-acquiring', 'stopped');
        $('#s'+uuid+' ul li.acquire-state span .value').text('stopped');
        $btn.html("<i class='fa fa-circle-o'></i> Start logging");
        $btn.attr('title', 'Start logging');
        $('#sensors').find('#s'+uuid).addClass('inactive');
    }
    $btn.attr('data-active', ''+is_active);
}

function set_sensor_name(uuid, name){
    var $text = $('#s'+uuid+" .sensor-name h3");
    $text.text(name);
}

function update_channel_sparkline(uuid, channel_id) {
    $.ajax("/devices/"+uuid+"/sensors/"+channel_id+"/samples/json", {
        success: function (data, error, xhr) {
            var span_id = '#s'+uuid+' ul.channels [data-channel-id="'+channel_id+'"] .sparkline';
            sparkline_set_data(span_id, data);
            if (xhr.status == 202)
                setTimeout(update_channel_sparkline, 1000, uuid, channel_id);
        }
    });
};

function update_channel_sparklines(uuid) {
    $("#s"+uuid+" ul#channels li").each(function(i, $el) {
        var channel = $el.attr('data-channel-id');
        update_channel_sparkline(uuid, channel);
    });
}

function add_sensor_row(uuid, sensor_name) {
    var $sensor = $("<li></li>", {
        id: 's'+uuid,
        class: ['sensor']
    });
    
    var $del_sensor_btn = $("<button class='btn btn-warning btn-xs btn-delsensor'/>")
        .attr('title', 'delete sensor from table')
        .append("<i class='fa fa-times'></i> Remove")
        .click(function (event) {
            event.preventDefault();
            delete_row(uuid);
            eject_sensor(uuid);
        });

    var $row = $sensor.append('<div/>');
    $row.append(
        $('<span/>')
            .addClass('sensor-name')
            .append($('<h3/>').text(name))
            .append($("<button><i class='fa fa-pencil'></i></button>")
                    .addClass("edit-btn btn btn-default btn-xs")
                    .click(function () {
                        var $sensor = $('#s'+uuid)
                        var $editBtn = $sensor.find("span.sensor-name .edit-btn");
                        var $text = $sensor.find("span.sensor-name h3");
                        edit_text($text, $editBtn, function(name) {
                            sensor_name_change(uuid, name);
                        });
                    })
                   )

            .append($('<ul/>')
                    .addClass('actions')
                    .append($('<li/>').append($del_sensor_btn))
                   )
        );

    var $acquire_btn =
        $('<button/>')
            .addClass("activate-btn")
            .addClass("btn btn-xs btn-primary")
            .attr("data-active", 'false')
            .append($('<i/>'))
            .click(function () {
                var is_acquiring = $acquire_btn.attr("data-active") == 'true';
                var start_acquiring = !is_acquiring;
                start_stop_acquire(uuid, start_acquiring);
                set_status_active(uuid, !is_acquiring);
            });

    var $config = $("<span/>")
        .append($("<span class='configuration-state'></span>"))
        .append($("<button><i class='fa fa-cog'></i> Configure</button>")
                .addClass('btn btn-primary btn-xs configure-btn')
                .click(function (event) {
                    var t = parseFloat($("#sample-interval").val()) * 60 * 1000;
                    $.ajax("/devices/"+uuid+"/sample-period", {
                        type: "POST",
                        data: {value: t},
                        success: function (data, status, xhr) {
                            $("#s"+uuid+" .configuration-state").html("Configured");
                        }
                    });
                })
               );

    var $row = $sensor.append('<div/>');
    $row.append($("<ul/>")
                .addClass('actions')
                .append($('<li/>')
                        .addClass('acquire-state')
                        .append('<span class="meta">Currently the sensor is <span class="value">unknown</span>.</span> ')
                        .append($acquire_btn))
                .append($('<li/>').append($config))
               );

    function do_plot() {
        $.ajax('/devices/'+uuid+'/samples/json', {
            success: function (data, error, xhr) {
                filtered = [];
                for (i in data) {
                    data[i].time *= 1000; // times expected to be in milliseconds
                    if (data[i].sensor == 1)
                        filtered.push(data[i]);
                }
                curve_set_data(filtered);
                update_channel_sparklines();
            }
        });
    }

    $data_controls = $("<ul/>")
                     .addClass('actions');
    $data_controls.append(
        $('<li/>').append($("<span class='sample-count meta'>Currently <span class='value'>unknown</span> samples are stored on the sensor.</span>"))
    );

    $data_controls.append(
        $("<li/>")
        .append($("<button class='btn btn-xs btn-primary plot-btn' />")
                .append("<i class='fa fa-bar-chart-o'></i> Plot")
                .click(do_plot)
               )
    );

    $data_controls.append(
        $("<li/>")
        .append($("<button class='btn btn-xs btn-primary download-btn'/>")
                .append("<i class='fa fa-download'></i> CSV")
                .click(function () {
                    location.href = "/devices/"+uuid+"/samples/csv";
                })
               )
    );

    $data_controls.append(
        $('<li/>')
        .append($("<button class='btn btn-xs btn-danger delete-btn'/>")
                .click(function (event) {
                    $.ajax("/devices/"+uuid+"/erase", {
                        type: "POST",
                        success: function (data, status, xhr) {
                            get_status(uuid);
                        }
                    });
                })
                .append("<i class='fa fa-trash-o'></i> Erase")
               )
    );

    var $row = $sensor.append('<div/>');
    $row.append($data_controls);

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
                update_channel_sparkline(uuid, channel.sensor_id);
            }
        }
    });
    var $row = $sensor.append('<div/>');
    $row.append(channels);

    $("#sensors").append($row);

}
