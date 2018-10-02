alfa=['ABCDEFHIJKLMNOPRSTUVWXYZ*'];
numlett=[1:25]
num=size(numlett)
colonnalink=[]
colonnaid=[]
colonnanomi=[]
for i=1:25
    for j=1:30
   html=webread(strcat(strcat(strcat(strcat(strcat('https://itunes.apple.com/us/genre/ios-medical/id6020?mt=8&letter=', alfa(1,i), '&page=', j, '#page'))))))
 
   nomi=regexp(html,'?mt=8">(.+?)</a> </li>','tokens')
       colonnanomi=[colonnanomi;nomi']
        
                 link=regexp(html, ' <li><a href="(.+?)">','tokens')
                 colonnalink=[colonnalink;link']
    
                          id=regexp(html, '/id(.+?)?mt=8">','tokens')
                          colonnaid=[colonnaid;id']
                          
                          TabellaMedical=[colonnanomi,colonnalink,colonnaid]
                          
    end
end
