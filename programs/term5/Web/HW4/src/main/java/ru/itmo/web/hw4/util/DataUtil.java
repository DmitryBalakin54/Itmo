package ru.itmo.web.hw4.util;

import ru.itmo.web.hw4.model.Post;
import ru.itmo.web.hw4.model.User;

import javax.servlet.http.HttpServletRequest;
import java.util.*;

public class DataUtil {
    private static final List<User> USERS = Arrays.asList(
            new User(1, "MikeMirzayanov", "Mike Mirzayanov", User.Color.RED),
            new User(6, "pashka", "Pavel Mavrin", User.Color.BLUE),
            new User(9, "geranazavr555", "Georgiy Nazarov", User.Color.GREEN),
            new User(11, "tourist", "Gennady Korotkevich", User.Color.RED)
    );

    private static final List<Post> POSTS = Arrays.asList(
            new Post(1, "Codeforces Round #510 (Div. 2)", "Hello, Codeforces! Codeforces Round #510 (Div. 2) will start on Monday, September 17, 2018 at 11:05. This round is rated for Div. 2 contestants.", 1),
            new Post(2, "Other Title 1", "Inceptos gravida dictumst ultrices, sem venenatis euismod scelerisque vitae? Laoreet sed tortor velit, erat blandit ante cras cum. Habitant, magna erat blandit. Ut ultrices ridiculus aptent dapibus mauris erat. Potenti ligula etiam praesent arcu! Gravida erat id faucibus accumsan nisl egestas. Tristique netus in interdum primis volutpat metus nibh auctor nisi! Vulputate euismod sapien dolor bibendum natoque at rhoncus pharetra. Laoreet, varius risus enim donec!", 6),
            new Post(3, "Other Title 2", "A short post content.", 9),
            new Post(4, "Other Title 3", "Gravida curae; elit senectus eros sem ullamcorper integer cras viverra aenean imperdiet. Taciti cum pellentesque placerat lorem quis. Inceptos sem auctor lacus dignissim cras lacinia magnis tincidunt condimentum. Interdum nostra mauris enim placerat, ligula felis porttitor tristique nam euismod. Imperdiet sem mattis placerat fermentum consectetur tristique duis rutrum lorem sociosqu aliquam. Nisi orci nascetur.\n" +
                    "\n" +
                    "Placerat bibendum maecenas per ultrices dolor bibendum. Turpis a luctus orci id. Posuere, fringilla interdum himenaeos lobortis rutrum consectetur fusce risus hendrerit. Vel at metus faucibus arcu euismod posuere sit. Fames elementum, sociis cubilia. Torquent pharetra nibh litora lacinia purus quisque dictumst euismod euismod molestie. Iaculis consequat magna, elit nostra. Parturient justo tortor bibendum habitant aptent vitae. Penatibus nascetur enim velit lectus praesent primis ut. Blandit massa, parturient senectus sem venenatis nascetur. Senectus nec dignissim pretium curae; magna duis hendrerit. Proin penatibus ipsum quisque sit pulvinar tincidunt duis euismod blandit. Molestie felis etiam nec montes.", 11),
            new Post(5, "Exploring the Universe", "Space exploration has always fascinated humanity. As we look up into the night sky, we can only imagine the vastness of the universe and the mysteries it holds. Scientists and astronomers continue to uncover secrets about distant planets and stars. One of the most significant advancements in recent years has been the discovery of exoplanets, which are planets outside our solar system. Some of these planets have conditions that might support life. The quest for understanding our universe is more critical than ever, and it is a journey that captivates everyone, from children dreaming of becoming astronauts to seasoned scientists.", 1),
            new Post(6, "The Future of Technology", "The future of technology is an exciting topic that constantly evolves. With advancements in artificial intelligence, machine learning, and quantum computing, we are entering a new era that promises to change how we live and work. Technologies such as self-driving cars, smart homes, and virtual reality are not just concepts of science fiction anymore; they are becoming part of our everyday lives. However, with these advancements come challenges such as privacy concerns and the need for ethical considerations. As we embrace these changes, it is essential to ensure that technology serves humanity positively and responsibly.", 6),
            new Post(7, "Healthy Living in the Modern World", "In today's fast-paced world, maintaining a healthy lifestyle can be a daunting challenge. With the rise of processed foods and sedentary habits, many people struggle to prioritize their health. However, adopting simple changes can significantly impact overall well-being. Incorporating more fruits and vegetables into your diet, engaging in regular physical activity, and practicing mindfulness are effective strategies to enhance health. Moreover, the importance of mental health cannot be overlooked; taking time to relax and de-stress is crucial in maintaining a balanced life. Embracing a holistic approach to health is key in navigating modern life's complexities.", 9),
            new Post(8, "Traveling the World: A Journey of Discovery", "Traveling is one of the most enriching experiences one can have. It opens the door to new cultures, perspectives, and ways of life. Whether exploring ancient ruins, tasting exotic cuisines, or meeting new people, travel offers endless opportunities for personal growth and discovery. However, it is essential to travel responsibly and respectfully, understanding the impact of tourism on local communities and the environment. Sustainable travel practices are becoming increasingly important in preserving the beauty of our planet for future generations. So pack your bags, and embark on a journey that could change your life forever!", 9),
            new Post(9, "The Art of Storytelling", "Storytelling is an age-old tradition that transcends cultures and generations. It is a powerful tool for communication that can inspire, educate, and entertain. Through storytelling, we share our experiences, values, and dreams with others. In today's digital age, the art of storytelling has evolved, with new mediums such as podcasts, blogs, and social media platforms allowing individuals to reach wider audiences. Whether through written word or spoken narrative, the essence of storytelling remains the same: connecting with others through shared experiences and emotions. Embrace the art of storytelling and let your voice be heard!", 11)

    );

    public static void addData(HttpServletRequest request, Map<String, Object> data) {
        data.put("users", USERS);
        data.put("posts", POSTS);

        List<Post> sideBarPosts = DataUtil.getRandomPosts(POSTS, 3);
        data.put("sideBarPosts", sideBarPosts);

        for (User user : USERS) {
            if (Long.toString(user.getId()).equals(request.getParameter("logged_user_id"))) {
                data.put("user", user);
            }
        }
    }

    public static List<Post> getRandomPosts(List<Post> posts, int count) {
        List<Post> newPosts = new ArrayList(posts);
        Collections.shuffle(newPosts);
        return newPosts;
    }
}
