#lang scribble/manual
@require[@for-label[regolith
                    racket/base]]

@title{Regolith}
@author[(author+email "Oliver Strickson" "ostrickson@turing.ac.uk")
        (author+email "James Geddes"     "jgeddes@turing.ac.uk")]

@defmodule[regolith]{Produce a monthly recharge @italic{pro forma} detailing
REG's time allocation to projects}

The main job of @racketmodname[regolith] is to compute an appropriate allocation
for each month. This allocation is not entirely trivial because REG allocations
align on week boundaries, Forecast can align down to days, and PMU work on
calendar months. The purpose of this documentation is to explain what
@racketmodname[regolith] does.

The main properties of the system are as follows:
@itemlist[#:style 'ordered
@item{A REG month is a whole number of weeks.

In the REG system, a week consists of the days from Monday to Sunday and is
supposed to be the smallest unit of allocation. The entirety of a week is
allocated to the month in which the Thursday falls.Thus, some months are exactly
four weeks long, and some are exactly five weeks long. A year is either 52 or 53
weeks. All of this is to be consistent with the ISO week date system.

To be consistent, REG project allocations begin on Mondays and end on Sundays.}
@item{PMU work in terms of calendar months.

For PMU, projects begin on the first of a month (and typically end on the last
day). There are often restrictions meaning that money cannot be ``carried over''
from one fiscal year to the next.}
@item{Some projects end in the middle of a month.}
]

Our problem is:

@nested[#:style 'inset]{Assign, to each month, an allocation percentage, having the
following two properies:

(S). The sum of all allocations for an individual who is fully allocated over a
year will be exactly 12.

(C). If a person is allocated by a simple number---say 50%---for two REG-months,
then that person should have a reported allocation of 50% for the corresponding
two calendar months (and no other months). This last constraint means that our
allocations match what PMU expect.}

The current process produces numbers which satisfy (S) and (C). The process is
as follows. To determine the reported allocation for month M:
@itemlist[#:style 'ordered

@item{Generate a date interval that corresponds to the REG definition of M. That
is, construct a consecutive sequence of days starting on a Monday and ending on
a Sunday such that every Thursday in the range belongs to calendar month M.}

@item{Compute the number of weekdays (Monday to Friday inclusive) in the
intersection of this interval with the allocation.}

@item{Pro-rate the allocation by the ratio of the number of weekdays in the
interval to the number of weekdays in the month. (Note that the number of
weekdays in the month will be either 20 or 25 exactly.)}]



