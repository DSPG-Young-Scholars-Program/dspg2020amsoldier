// !preview r2d3 data=c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20)
//
// r2d3: https://rstudio.github.io/r2d3
//

var vw = width; // get viewport width
var vh = height; // get viewport height

var data;
var tooltip;

var minDate, maxDate;
var startX = 0;

var margin = {
    top: 20,
    bottom: 0,
    left: 25,
    right: 25,
};

var canvasWidth = width;
var canvasHeight = height;

var cv_Y = margin.top + 25; // y value of civil rights line
var mi_Y = height - margin.bottom - 50; // y value of military line
var in_Y = (cv_Y + mi_Y)/2; // y value of intersection line

var groups = {
    CIVIL_RIGHTS: "Civil Rights",
    MILITARY: "Military",
    INTERSECTION: "Intersection"
}

var xScale, xAxis;
var curYear, yearStart, yearEnd;
var svg, canvas, legend;
var xBinWidth;


function createTooltip() {
    // create tooltip
    var tooltip = d3.select('body')
        .append('div')
        .attr('id', 'tooltip')
        .style('opacity', '0');
}

function reformatData() {
    var startDates = data.map(d => {
        var date = new Date(d['Date']);
        return date;
    });

    var endDates = data.map(d => {
        var date = new Date(d['End']);
        return date;
    });

    minDate = new Date(Math.min(...startDates));
    maxDate = new Date(Math.max(...endDates));
}

function draw() {
    createTooltip();
    reformatData();

    // create x scale, converts Date value to respective place on chart
    xScale = d3.scaleTime()
        .domain([minDate, maxDate])
        .range([margin.left, canvasWidth - margin.right]);

    xAxis = svg.append('g')
        .attr("transform", `translate(0, ${30})`)
        .attr('class', 'xAxis')
        .call(d3.axisTop(xScale));

    d3.select('.xAxis')
        .selectAll("text")
        .style('font-size', '20px');

    createClipPath();
    canvas = svg.append('g')
        .attr('clip-path', 'url(#clip)');
    drawBaseLines();
    connectToBaseLines(xScale);

    var zoom = d3.zoom()
        .scaleExtent([.5, 20])
        .extent([[0, 0], [canvasWidth, canvasHeight]])
        .on("zoom", updateChart);

    createWW2Box();
    drawWW2Box(xScale);
    drawLegend();

    canvas.append('rect')
        .attr('width', canvasWidth)
        .attr('height', canvasHeight)
        .style('fill', 'none')
        .style('pointer-events', 'all')
        .attr('transform', `translate(${margin.left}, ${margin.top})`)
        .call(zoom);

    drawCircles();
}

function updateChart() {
    var newX = d3.event.transform.rescaleX(xScale);
    xAxis.call(d3.axisTop(newX));

    // update circle positions
    canvas.selectAll("circle")
        .attr('cx', (d) => {
            return newX(new Date(d['Date']));
        });

    // remove old line connections
    canvas.selectAll(".cvConnectionLine").remove();
    canvas.selectAll(".miConnectionLine").remove();

    // draw new line connections
    connectToBaseLines(newX);

    // update position of ww2-box
    drawWW2Box(newX);
}

function drawBaseLines() {
    var baseLineGenerator = d3.line();

    // draw civil rights base line
    var cvStart = [margin.left, cv_Y];
    var cvEnd = [canvasWidth - margin.right, cv_Y];
    var cvBasePoints = [cvStart, cvEnd];
    var cvPath = baseLineGenerator(cvBasePoints);

    // append civil rights base line to chart
    canvas.append('path')
        .attr("d", cvPath)
        .attr("class", "groupLine cvLine");

    // draw military base line
    var miStart = [margin.left, mi_Y];
    var miEnd = [canvasWidth - margin.right, mi_Y];
    var miBasePoints = [miStart, miEnd];
    var miPath = baseLineGenerator(miBasePoints);

    // append military base line to chart
    canvas.append('path')
        .attr("d", miPath)
        .attr("class", "groupLine miLine");
}

function generateCivilRightsPoints() {
    var start = [margin.left, cv_Y];
    var cv_points = [];
    cv_points.push(start);
    data.sort((a, b) => {
        var d1 = new Date(a['Date']);
        var d2 = new Date(b['Date']);
        return d1.getTime() - d2.getTime();
    }).forEach((d) => {
        var group = d['Group'];
        var px = xScale(new Date(d['Date']));
        var py;
        if (group == groups.CIVIL_RIGHTS) {
            py = cv_Y;
            cv_points.push([px, py]);
        } else if (group == groups.INTERSECTION) {
            py = in_Y;
            cv_points.push([px, py]);
        }
    });
    var end = [canvasWidth - margin.right, cv_Y];
    cv_points.push(end);
    return cv_points;
}

function createClipPath() {
    var clip = svg.append('defs').append("SVG:clipPath")
        .attr('id', 'clip')
        .append('SVG:rect')
        .attr('width', width)
        .attr('height', height)
        .attr('x', 0)
        .attr('y', 0);
    return clip;
}

function generateMilitaryPoints() {
    var start = [margin.left, mi_Y];
    var mi_points = [];
    mi_points.push(start);
    data.sort((a, b) => {
        var d1 = new Date(a['Date']);
        var d2 = new Date(b['Date']);
        return d1.getTime() - d2.getTime();
    }).forEach((d) => {
        var group = d['Group'];
        var px = xScale(new Date(d['Date']));
        var py;
        if (group == groups.MILITARY) {
            py = mi_Y;
            mi_points.push([px, py]);
        } else if (group == groups.INTERSECTION) {
            py = in_Y;
            mi_points.push([px, py]);
        }
    });
    var end = [canvasWidth - margin.right, mi_Y];
    mi_points.push(end);
    return mi_points;
}

function drawCircles() {
    var circles = canvas.selectAll("circle")
        .data(data);

    circles.enter().append("circle")
        .merge(circles)
        .attr("cx", (d) => {
                var date = new Date(d["Date"]);
                var x = xScale(date);
                return x;
            })
            .attr("cy", (d) => {
                var group = d['Group'];
                var y;
                switch(group) {
                    case groups.CIVIL_RIGHTS:
                        y = cv_Y;
                        break;
                    case groups.MILITARY:
                        y = mi_Y;
                        break;
                    case groups.INTERSECTION:
                        y = in_Y;
                        break;
                };
                return y;
            })
            .attr("r", (d) => {
                var r = 10;
                return r;
            })
            .attr("class", (d) => {
                var className = "event";
                if (d["Event Name"] == "Survey 32") {
                    className += " survey32";
                }
                return className;
            })
            .on("mouseover", highlight)
            .on("mouseout", unhighlight)
            .on("click", (d) => {
                window.location.href = d['Source'];
            });
}

function connectToBaseLines(scale) {
    var connectionOffset = 0;
    var lineGenerator = d3.line().curve(d3.curveCatmullRom);
    data.forEach(d => {
        var group = d['Group'];
        if (group === groups.INTERSECTION) {
            var eventX = scale(new Date(d["Date"]));

            // connect to cv line
            var cvConnectionPoints = [
                [eventX - connectionOffset, cv_Y],
                [eventX - connectionOffset/2, cv_Y + 10],
                [eventX, in_Y]
            ];
            var cvConnectionPath = lineGenerator(cvConnectionPoints);
            canvas.append('path')
                .attr("d", cvConnectionPath)
                .attr("class", "groupLine cvConnectionLine");

            // connect to mi line
            var miConnectionPoints = [
                [eventX - connectionOffset, mi_Y],
                [eventX - connectionOffset/2, mi_Y - 10],
                [eventX, in_Y]
            ];
            var miConnectionPath = lineGenerator(miConnectionPoints);
            canvas.append('path')
                .attr("d", miConnectionPath)
                .attr("class", "groupLine miConnectionLine");
        }
    });
}

function highlight(d) {
    // draw yellow circle around selected circle
    var circle = d3.select(this)
        .style("stroke","yellow")
        .style("stroke-width","3");

    var html = tooltipHTML(d);

    d3.select('#tooltip')
        .html(html)
        .style("left", (d3.event.pageX + 20) + "px")
        .style("top", (d3.event.pageY - 20) + "px")
        .transition()
            .duration(300)
            .style('opacity', 0.9);
}

function unhighlight(d) {
    var circle = d3.select(this)
        .style("stroke","")
        .style("stroke-width","");

    d3.select('#tooltip')
        .transition()
            .duration(300)
            .style("top", "1000px")
            .style('opacity', 0)
}

function tooltipHTML(d) {
    return (`<div id="tooltip-content"><span id="tooltipTitle">${d['Event Name']}</span><p>${d['Description']}</p></div>`);
}

function drawLines() {
    var durationLineGenerator = d3.line();
    data.forEach(d => {
        var startDate = new Date(d["Date"]);
        var endDate = new Date(d["End"]);

        if (startDate !== endDate) {
            var x0, x1, y;
            y = getYFromGroup(d['Group']);
            x0 = xScale(startDate);
            x1 = xScale(endDate);
            var points = [
                [x0, y],
                [x1, y]
            ];

            var pathData = durationLineGenerator(points);
            canvas.append('path')
                .attr('d', pathData)
                .attr("stroke", "black")
                .attr("stroke-width", 2);
        }
    });
}

function getYFromGroup(group) {
    switch(group) {
        case groups.CIVIL_RIGHTS:
            return cv_Y;
        case groups.MILITARY:
            return mi_Y;
        case groups.INTERSECTION:
            return in_Y;
    }
}

function drawLegend() {
    var legendWidth = 400;
    var legendHeight = 30;
    var lineGenerator = d3.line();

    // draw cv line legend
    var cvLineLegendPoints = [
        [width - legendWidth, height - legendHeight],
        [width - legendWidth + 10, height - legendHeight]
    ];
    svg.append('path')
        .attr('d', lineGenerator(cvLineLegendPoints))
        .attr('class', 'groupLine cvLine');
    svg.append('text')
        .attr("x", cvLineLegendPoints[1][0] + 5)
        .attr("y", cvLineLegendPoints[0][1] + 5)
        .attr("class", "legendText")
        .text("related to race");

    // draw mi line legend
    var miLineLegendPoints = [
        [canvasWidth - legendWidth, canvasHeight - legendHeight + 20],
        [canvasWidth - legendWidth + 10, canvasHeight - legendHeight + 20]
    ];

    svg.append('path')
        .attr('d', lineGenerator(miLineLegendPoints))
        .attr('class', 'groupLine miLine');

    svg.append('text')
        .attr("x", miLineLegendPoints[1][0] + 5)
        .attr("y", miLineLegendPoints[0][1] + 5)
        .attr("class", "legendText")
        .text("related to the military");

    // draw survey 32 legend component
    svg.append('circle')
        .attr('cx', canvasWidth - legendWidth/2)
        .attr('cy', canvasHeight - legendHeight)
        .attr('r', 5)
        .attr('class', 'event survey32');

    svg.append('text')
        .attr('x', canvasWidth - legendWidth/2 + 10)
        .attr('y', canvasHeight - legendHeight + 5)
        .attr('class', 'legendText')
        .text('Survey 32');

    // draw ww2 box legend component
    svg.append('rect')
        .attr('x', canvasWidth - legendWidth/2 - 5)
        .attr('y', canvasHeight - legendHeight + 15)
        .attr('height', 10)
        .attr('width', 10)
        .style("opacity", 0.1);


    svg.append('text')
        .attr('x', canvasWidth - legendWidth/2 + 10)
        .attr('y', canvasHeight - legendHeight + 25)
        .attr('class', 'legendText')
        .text('WWII');



}

function drawWW2Box(scale) {
    // start of WW2: 1 September 1939
    var startDate = new Date(1939, 9, 1);

    // end of WW2: 2 September 1945
    var endDate = new Date(1945, 9, 2);

    // width of box
    let width = scale(endDate) - scale(startDate);

    canvas.select('#ww2-box')
        .attr("x", scale(startDate))
        .attr("y", margin.top + 10)
        .attr("width", scale(endDate) - scale(startDate))
        .attr("height", canvasHeight - margin.top - margin.bottom - 50)
        .on("mouseover", () => {
            d3.select("#ww2-box")
                .attr("stroke", "yellow")
                .attr("stroke-width", "3");
        })
        .on("mouseout", () => {
            d3.select("#ww2-box")
                .attr("stroke", "")
                .attr("stroke-width", "");
        });
}

function createWW2Box() {
    canvas.append('rect').attr('id', 'ww2-box');
}

draw();

