 //On click, update with new data			
    d3.selectAll("#submitchanges")
        .on("click", function () {

            var checkedboxes = $('input[type=checkbox]:checked')

            var checkboxlist = []
            for (var i = 0; i < checkedboxes.length; i++) {
                checkboxlist[i] = $(checkedboxes[[i]]).val()
            };
            console.log(checkboxlist);

            // update the dataset we are using          
            for (i in ed_data) {
                if (checkboxlist.includes(data[i].key)) {
                    data[i].name_filter = "keep";
                } else {
                    data[i].name_filter = "remove";
                }
            };


            ed_data_filtered = ed_data.filter(d => d.name_filter == "keep");


            var division_cols = viz_box.selectAll(".column")
                .data(ed_data_filtered, key);

            division_cols.exit()
                .transition()
                .duration(1000)
                .attr("width", 0)
                .remove();

            var entering_cols = division_cols.enter()
                .append("div")
                .classed("column", true)
                //        .classed("col-sm-2", true)
                .attr("id", (d) => "column_" + d.key);

            entering_cols.append("h3").text((d) => d.values[[0]].values[[0]].values[[0]].division_name);

            var cohort_boxes = entering_cols.selectAll(".cohort_box")
                .data(d => d.values)
                .enter()
                .append("div")
                .classed("cohort_box", true)
                .attr("id", (d) => "cohort" + d.key);

            var cohort_svg = cohort_boxes
                .append("svg")
                .attr("viewBox", "0 0 " + usewidth + " " + useheight)
                .attr("class", "svg-content")
                .attr("id", (d) => d.key)
                .attr("aria-labelledby", "title");

            var graph_containers = cohort_svg.append("g")
                .attr("transform", "translate(" + margin.left + "," + margin.top + ")")
            //
            //
            //
            var gradient_containers = graph_containers.append("g")
                .classed("gradient_containers", true)
                .attr("id", function (d) {
                    return "gradient_group" + d.key + d.values[[0]].values[[0]].division_use
                })
                .attr("clip-path", function (d) {
                    return "url(#area" + d.key + d.values[[0]].values[[0]].division_use + ")"
                });
            //
            var negative = gradient_containers.append("g")
                .attr("id", function (d) {
                    return "gradientBlack" + d.key
                })
                .selectAll(".negative_rects")
                .data(number_scale)
                .enter()
                .append("rect")
                .attr("x", 0)
                .attr("y", (d) => color_scale(d + 1))
                .attr("width", width)
                .attr("height", (d) => color_scale(d))
                .style("fill", (d) => negative_color(d))
                .classed("negative_rects backrect", true);

                        var positive = gradient_containers
                            .append("g")
                            .attr("id", function (d) {
                                return "gradientWhite" + d.key
                            })
                            .attr("clip-path", function (d) {
                                return "url(#pathAll" + d.key + d.values[[0]].values[[0]].division_use + ")"
                            }) // Divide the positive from the negative gradient along the average line. 
                            .selectAll(".positive_rects")
                            .data(number_scale).enter().append("rect")
                            .attr("x", 0)
                            .attr("y", (d) => color_scale(d + 1))
                            .attr("width", width)
                            .attr("height", (d) => color_scale(d))
                            .style("fill", (d) => positive_color(d))
                            .classed("positive_rects backrect", true);

                        gradient_containers
                            .selectAll(".seperators")
                            .data((d) => d.values.filter(function (el) {
                                return el.key === "All"
                            }))
                            .enter()
                            .append("clipPath")
                            .attr("id", function (d) {
                                return "path" + d.key + d.values[[0]].cohort + d.values[[0]].division_use
                            })
                            .append("path")
                            //       .attr("class", function(d){ return "areas " + d.key})
                            .attr("d", (d) => gradient_area(d.values));


          //   create the path clippings to define the race difference areas. 
                        var gap_area = d3.area()
                            .defined(function (d) {
                                return d;
                            })
                            .x(function (d) {
                                return xScale(+d.grade);
                            })
                            .y0(function (d) {
                                return yScale(+d.black);
                            })
                            .y1(function (d) {
                                return yScale(+d.pass_rate);
                            })
            //
            //
            gradient_containers
                            .selectAll(".areas")
                            .data((d) => d.values.filter(function (el) {
                                return el.key === "White"
                            }))
                            .enter()
                            .append("clipPath")
                            .attr("id", function (d) {
                                return "area" + d.values[[0]].cohort + d.values[[0]].division_use
                            })
                            .append("path")
                            //       .attr("class", function(d){ return "areas " + d.key})
                            .attr("d", (d) => gap_area(d.values));
            
                        var race_color = d3.scaleOrdinal().domain(["White", "All", "Black"]).range(["#33b3a6", "black", "rgb(194, 50, 10)"])

            // Add in dots

            graph_containers
                .selectAll(".dotcontainer")
                .data((d) => d.values)
                .enter()
                .append("g")
                .attr("class", function (d) {
                    return "dotcontainer dots" + d.key
                })
                .selectAll(".dots")
                .data((d) => d.values)
                .enter()
                .append("circle")
                .attr("cx", function (d) {
                    return xScale(+d.grade);
                })
                .attr("cy", function (d) {
                    return yScale(+d.pass_rate);
                })
                .attr("r", 5)
                .style("fill", function (d) {
                    return race_color(d.race)
                });


            // Add in lines 

            // Create the path clippings to divide the positive from the negative gradient
            var lines = d3.line()
                .defined(function (d) {
                    return d;
                })
                .x(function (d) {
                    return xScale(+d.grade);
                })
                .y(function (d) {
                    return yScale(+d.pass_rate);
                })

            graph_containers
                .selectAll(".linecontainer")
                .data((d) => d.values)
                .enter()
                .append("g")
                .attr("class", function (d) {
                    return "linecontainer linescontainer" + d.key
                })
                .append("path")
                .attr("d", (d) => lines(d.values))
                .attr("class", function (d) {
                    return "lines line" + d.key
                })
                .attr("stroke", function (d) {
                    return race_color(d.key)
                });

            // Grades Scale
            graph_containers.append("g")
                .attr("class", "axis")
                .call(d3.axisTop().scale(xScale).tickValues([3, 4, 5, 6, 7, 8]).tickFormat(d3.format(".1")))
                .attr("transform", "translate(" + 0 + "," + 0 + ")");
            //    
            // graph_containers.append("g")
            //    .call(d3.axisLeft().scale(yScale))
            //    .attr("transform", "translate(" + 0 + "," + height + ")"); 


            // Add Gridlines

            yvals = [50, 90];

            graph_containers.selectAll(".gridlines")
                .data(yvals)
                .enter()
                .append("line")
                .attr("x1", xScale(2.95))
                .attr("x2", xScale(8.05))
                .attr("y1", (d) => yScale(d))
                .attr("y2", (d) => yScale(d))
                .attr("class", "gridline")

            graph_containers.selectAll(".gridlabels")
                .data(yvals)
                .enter()
                .append("text")
                .attr("class", "gridlabels")
                .text((d) => d + "%")
                .attr("x", xScale(2.9))
                .attr("y", (d) => yScale(d));



            cohort_svg
                .append("text")
                .text((d) => d.key)
                .attr("class", "yeartext")
                .attr("transform", "translate(" + usewidth / 2 + "," + useheight / 2 + ")")


        });
