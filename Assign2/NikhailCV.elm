module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


sectionStyle = style [("margin","0px 0px 0px 0px"),("padding","0px 0px 0px 0px")]
python = style [("width", "90%")]
haskell = style [("width", "70%")]
visualBasic = style [("width","80")]
serviceNow = style [("width", "75%")]
java = style [("width", "70%")]
cStyle = style [("width", "85%")]
office = style [("width", "98%")]
photoshop = style [("width", "60%")]
frame = style [("width","80%"),("padding", "20px 30px 35px 70px"),("Background", "Black")]
sidebar = style [("float","left")]
view model =
  body [class "body"] 
  [
    div [class "Wrapper",frame]
        [
            div[class "sidebar-wrapper",sidebar]
            [
             div [class "profile-container"]
              [
               img [class "profile",src "Nikhail-ico.png"] [],
               h1 [class "name"] [text "Nikhail Singh"],
               h3 [class "tagline"] [text "Computer Science Student"]
              ],  -- sidebar container
             div [class "contact-container container-block"]
              [
                ul [class "list-unstyled contact-list"]
                    [
                     li [class "email"][ i [ class "fa fa-envelope"][], a [href "mailto: yourname@email.com"] [text "singhn18@mcmaster.ca"]],
                     li [class "phone"][ i [ class "fa fa-phone"][], a [href "tel 0123 456 789"] [text "647 702 1134"]],
                     li [class "linkedin"][ i [ class "fa fa-linkedin"][], a [href "https://www.linkedin.com/in/nikhailsingh", target "_blank"] [text "in/nikhailsingh"]],
                     li [class "github"][ i [ class "fa fa-github"][], a [href "https://www.github.com/singhn18", target "_blank"] [text "github.com/singhn18"]]
                    ]
              ], --contact
             div [class "education-container container-block"]
              [
               h2 [class "container-block-title"] [text "Education"],
               div [class "item"]
                [
                 h4 [class "degree"] [text "BASc in Computer Science"],
                 h5 [class "meta"] [text "McMaster University"],
                 div [class "time"] [text "2017-2021"]
                ], --item
               div [class "item"]
                [
                 h4 [class "degree"] [text "High School Degree"],
                 h5 [class "meta"] [text "Martingrove Collegiate Institute"],
                 div [class "time"] [text "2016"]
                ] --item
              ],--education
             div [class "interests-container container-block"]
              [
               h2 [class "container-block-title"] [text "Interests"],
               ul [class "list-unstyled interests-list"]
                [
                 li [] [text "Hiking"],
                 li [] [text "Reading"],
                 li [] [text "Mentorship"]
                ]--list
              ]--interests
            ], --sidebar-wrapper
            div [class "main-wrapper"]
             [
              section [class "section summary-section"]
              [
               h2 [class "section-title"] [i[class "fa fa-user"][],a [] [text "About Me"]],
               div [class "summary"] [p [] [text "Hi, I'm Nikhail Singh a first year Computer Science student at McMaster University with a CGPA of 3.8/4.0. I'm extremely hard-working, dedicated, passionate and resourceful. I also pride myself on being a quick learner. In my spare time I do a lot of volunteering for various services around campus such as the Student Walk Home Attendant Team or the Spark Mentorship Program"]]
              ],
              section [class "section experiences-section"]
              [
               h2 [class "section-title"] [i[class "fa fa-briefcase"][], a [] [text "Experience"]],
               div [class "item"]
                [
                 div [class "meta"]
                  [
                   div [class "upper-row"]
                    [
                     h3 [class "job-title"] [text "Publicity Director"],
                     div [class "time"] [text "2015-2016"]
                    ],--upper row
                   div [class "company"] [text "Martingrove Engineering Team"]
                  ],--meta 
                 div [class "details"] 
                  [
                   p [sectionStyle] [text "-Team member of the first place Team at UofT’s Engineering Idol for designing & constructing a prosthetic limb"],
                   p [sectionStyle] [text "-Executive Member responsible for promoting weekly hardware & software design challenges & workshops"],
                   p [sectionStyle] [text "-Helped to manage and oversee events such as a Ted Talk Engineering Night with guest speakers"]
                  ]--detials
               ],--item
               div [class "item"]
                [
                 div [class "meta"]
                  [
                   div [class "upper-row"]
                    [
                     h3 [class "job-title"] [text "Publicity Director"],
                     div [class "time"] [text "2017-Present"]
                    ],--upper row
                   div [class "company"] [text "McMaster Students Union Student Walk Home Attendant Team"]
                  ],--meta 
                 div [class "details"] 
                  [
                   p [sectionStyle] [text "Working to promote and raise awareness for the SWHAT service on campus. Responsible for handling all public outreach including; designing promotional items, contacting sponsors, coordinating social media posts, and facilitating promotional events. Other responsibilities include managing walkers on shift and working with the coordinator to stay within budget lines."]
                  ]--detials
               ],--item
              div [class "item"]
                [
                 div [class "meta"]
                  [
                   div [class "upper-row"]
                    [
                     h3 [class "job-title"] [text "Team Leader"],
                     div [class "time"] [text "2017-Present"]
                    ],--upper row
                   div [class "company"] [text "McMaster Students Union Spark Mentorship Service"]
                  ],--meta 
                 div [class "details"] 
                  [
                   p [sectionStyle] [text "Working with a co-team leader to facilitate different sessions designed to help first year students better transition into university. Responsible for mentoring these students, being a positive role-model for them and listening to their diverse lived expereinces and providing support for them."]
                  ]--detials
               ]--item
              ],--experience
              section [class "section projects-section"]
               [
                h2 [class "section title"] [i[class "fa fa-archive"][], a[sectionStyle] [text " Projects"]],
                div [class "intro"] [p[sectionStyle] [text "Below is a list of some of the projects I have previosly completed"]],
                div [class "item"]
                 [
                  span [class "project-title"][a[href "anim.html", target "_blank",sectionStyle] [text "Animation Sandbox"]],
                  span [class "project-tagline"] [text " -A simple elm application that does different animations using SVG graphics. Click the title to view"]
                 ],
                div [class "item"]
                 [
                  span [class "project-title"][a[href "https://github.com/singhn18/CS1XA3/tree/master/Assign1" ,target "_blank",sectionStyle] [text "Bash project analyzer"]],
                  span [class "project-tagline"] [text " -A project analyzer written in Bash to help manage repositories. Click the title to view"]
                 ]
               ],--projects
              section [class "skills-section section"]
               [
                h2 [class "section-title"] [i [class "fa fa-rocket"][], a[sectionStyle][text "Skills & Proficieny"]],
                div [class "skillset"]
                 [
                  div [class "item"]
                   [
                    h3 [class "level-title"] [text "Python"],
                    div [class "level-bar"] [div [class "level-bar-inner",python][]]
                   ],
                  div [class "item"]
                   [
                    h3 [class "level-title"] [text "Haskell"],
                    div [class "level-bar"] [div [class "level-bar-inner",haskell][]]
                   ],
                  div [class "item"]
                   [
                    h3 [class "level-title"] [text "Visual Basic"],
                    div [class "level-bar"] [div [class "level-bar-inner",visualBasic][]]
                   ],
                  div [class "item"]
                   [
                    h3 [class "level-title"] [text "ServiceNow"],
                    div [class "level-bar"] [div [class "level-bar-inner",serviceNow][]]
                   ],
                  div [class "item"]
                   [
                    h3 [class "level-title"] [text "Photoshop"],
                    div [class "level-bar"] [div [class "level-bar-inner",photoshop][]]
                   ],
                  div [class "item"]
                   [
                    h3 [class "level-title"] [text "C++"],
                    div [class "level-bar"] [div [class "level-bar-inner",cStyle][]]
                   ],
                  div [class "item"]
                   [
                    h3 [class "level-title"] [text "Java"],
                    div [class "level-bar"] [div [class "level-bar-inner",java][]]
                   ],
                  div [class "item"]
                   [
                    h3 [class "level-title"] [text "Microsoft Office"],
                    div [class "level-bar"] [div [class "level-bar-inner",office][]]
                   ]
                 ]
               ]
             ]--main wrapper
            ],
  footer [class "footer"]
   [
    div [class "text-center"] [small [class "copyright"] [text "this CV was designed by Nikhail Singh for use in his CS 1XA3 assignment and was based off of Orbit a template made my Xiaoying Riley"]]
   ]
  ]
    

main =
    view "NikhailCV model"
