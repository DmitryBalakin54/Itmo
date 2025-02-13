export const data = {
    posts: [
        {
            id: 1,
            title: 'Post 1',
            text: 'abracadabra'
        },
        {
            id: 2,
            title: 'Post 2',
            text: 'abracadabra',
            comments: [

            ]
        },
        {
            id: 3,
            title: 'Post 3',
            text: 'abracadabra',
            comments: [
                {
                    id: 1,
                    author: 'author',
                    text: "text",
                    postId: 3
                }
            ]
        },
        {
            id: 4,
            title: 'Post 4',
            text: 'abracadabra',
            comments: [
                {
                    id: 1,
                    author: 'author',
                    text: "text",
                    postId: 4
                },
                {
                    id: 2,
                    author: 'author2',
                    text: "text2",
                    postId: 4
                }
            ]
        },
    ],
    users: [
        {
            id: 1,
            login: 'pavladog',
            name: 'Vlad Rozhko'
        },
        {
            id: 2,
            login: 'slovechkin',
            name: 'Slava Rybin'
        },
        {
            id: 3,
            login: 'ghstfc',
            name: 'Max Merkulov'
        },
    ]
}