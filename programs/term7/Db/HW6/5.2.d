HaveLecturerCourse(StudentId) :-
    Students(StudentId, _, GroupId),
    Lecturers(LecturerId, LecturerName),
    Marks(StudentId, CourseId, _),
    Plan(GroupId, CourseId, LecturerId),
    LecturerName = :LecturerName.

HaventLecturerCourse(StudentId) :-
    Students(StudentId, _, _),
    not HaveLecturerCourse(StudentId).