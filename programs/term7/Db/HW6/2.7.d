HaveCourse(StudentId) :-
    Students(StudentId, _, GroupId),
    Groups(GroupId, _),
    Plan(GroupId, CourseId, _),
    Courses(CourseId, CourseName),
    CourseName = :CourseName.

HaveMarkByCourseId(StudentId) :-
    Students(StudentId, _, _),
    Marks(StudentId, CourseId, _),
    Courses(CourseId, CourseName),
    CourseName = :CourseName.

StudentsByHaventMarkByCourseId(StudentId, StudentName, GroupName) :-
    Students(StudentId, StudentName, GroupId),
    Groups(GroupId, GroupName),
    not HaveMarkByCourseId(StudentId),
    HaveCourse(StudentId). 