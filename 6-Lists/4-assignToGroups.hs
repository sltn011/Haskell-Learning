players = ["Bob", "Tom", "Jim", "Kate", "Sue", "Joan", "Mike"]

assignToGroups people numGroups =
    zip people groups
    where groups = cycle [1 .. numGroups]