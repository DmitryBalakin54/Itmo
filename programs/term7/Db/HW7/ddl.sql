CREATE TABLE Groups (
    GroupId INT PRIMARY KEY,
    GroupName VARCHAR(10) NOT NULL
);

CREATE TABLE Students (
    StudentId INT PRIMARY KEY,
    StudentName VARCHAR(40) NOT NULL,
    GroupId INT NOT NULL
);

CREATE TABLE Clubs (
    ClubId INT PRIMARY KEY,
    ClubName VARCHAR(100) NOT NULL,
    ClubStudentHeadId INT NOT NULL
);

CREATE TABLE ClubMembers (
    ClubId INT NOT NULL,
    StudentId INT NOT NULL,
    PRIMARY KEY (ClubId, StudentId)
);

CREATE TABLE Lecturers (
    LecturerId INT PRIMARY KEY,
    LecturerName VARCHAR(50) NOT NULL
);

CREATE TABLE Courses (
    CourseId INT PRIMARY KEY,
    CourseName VARCHAR(60) NOT NULL
);

CREATE TABLE Plan (
    GroupId INT NOT NULL,
    CourseId INT NOT NULL,
    LecturerId INT NOT NULL,
    PRIMARY KEY (GroupId, CourseId)
);

CREATE TABLE Marks (
    StudentId INT NOT NULL,
    CourseId INT NOT NULL,
    Mark INT NOT NULL,
    PRIMARY KEY (StudentId, CourseId)
);
