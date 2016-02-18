compatible_axes = function(y1, y2) {
  # suggests two vertical axes for plotting ranges [0,y1] and [0,y2]
  # The ylim is selected from the sequence 0.5,1,2.5,5,10,25,50 such that
  # y1 is between (40%, 100%] of ylim1 and y2 is between (40%, 100%] of ylim2.
  # each axis can then be split into five equal bins and labeled with six tick marks,
  # e.g. 0, 5, 10, 15, 20, 25.
  c = c(1, 2.5, 5, 10) # one cycle
  lc = log10(c)
  y = c(y1, y2)
  ar = log10(y) # aspect ratio
  base = floor(ar)
  rem = ar - base # remainder
  ia = c(0, 0)
  for (i in 1:2) {
    if (rem[i]==lc[1]) ia[i] = 1
    else if (rem[i]<lc[2]) ia[i] = 2
    else if (rem[i]<lc[3]) ia[i] = 3
    else ia[i] = 4
  }
  ax = 10^base * c[ia]
}

plotbarline = function(ybar, yline, xlabels) {
  Pantone280 = rgb(0,39/255,118/255)
  Pantone363 = rgb(60/255,138/255,46/255)
  Gray50 = rgb(0.5,0.5,0.5)
  Gray70 = rgb(0.7,0.7,0.7)
  n = length(ybar)
  if (length(yline)!=n) stop("ybar and yline must be of the same length")
  if (length(xlabels)!=n) stop("xlabels must be of the same length as ybar and yline")
  ax = compatible_axes(max(ybar), max(yline))
  xa = 1:n
  ya0 = c(0,0.2,0.4,0.6,0.8,1)
  ya1 = ax[1]*ya0
  ya2 = ax[2]*ya0
  yl1 = as.character(ya1)
  yl2 = as.character(ya2)
  barlabels = as.character(ybar)
  linelabels = as.character(yline)
  plot.new()
  plot.window(xlim=c(0.5,n+0.5), ylim=c(0,1))
  # bottom axis
  ba = axis(side=1, at = xa, labels = xlabels, tick = TRUE, line = NA,
       pos = NA, outer = FALSE, font = NA, lty = "solid",
       lwd = 1, lwd.ticks = 1, col = Gray50, col.ticks = Gray50,
       hadj = NA, padj = NA)
  # left axis
  la = axis(side=2, at = ya0, labels = yl1, tick = TRUE, line = NA,
       pos = NA, outer = FALSE, font = NA, lty = "solid",
       lwd = 1, lwd.ticks = 1, col = Gray50, col.ticks = Gray50,
       hadj = NA, padj = NA)
  # right axis
  ra = axis(side=4, at = ya0, labels = yl2, tick = TRUE, line = NA,
       pos = NA, outer = FALSE, font = NA, lty = "solid",
       lwd = 1, lwd.ticks = 1, col = Gray50, col.ticks = Gray50,
       hadj = NA, padj = NA)
  abline(h=ya0, col=Gray70)
  ybaradj = ybar / ax[1]
  ylineadj = yline / ax[2]
  rect(xleft=xa-0.4, ybottom=rep(0,n), xright=xa+0.4, ytop=ybaradj,
       col = Pantone280, border = Pantone280, lty = "solid", lwd = 1)
  text(xa, ybaradj, barlabels, cex=1, pos=3, col=Pantone280)
  lines(x=xa, y=ylineadj, col = Pantone363, type="l", lwd=3)
  lines(x=xa, y=ylineadj, col = Pantone363, type="p", lwd=1, pch=15, cex=1.5)
  text(xa, ylineadj, linelabels, cex=1, pos=3, col=Pantone363)
  title(main="Binning profile of a_balance to target_ptb"
        , xlab="a_balance")
  #legend(1, 0.2, legend = c("event rate","count"))
}

freq = 400*c(0.9,0.4,0.7,0.8)
rate = 110*c(0.2,0.75,0.9,0.5)
xlab = c("[0,2.5)","[2.5,48)","[48,100)","[100,+INF)")

plotbarline(ybar = freq
            , yline = rate
            , xlabels = xlab)
