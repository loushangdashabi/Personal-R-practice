library(dagitty)
library(ggdag)

# 使用dagitty包当中的dagitty函数来创建DAG
# X，Y，Z这些字母都是随便的，你可以想用什么字母用什么，大写小写都可以
# 这些字母代表你的变量
# ->代表方向
# 每一个；表示一个具体的点边关系
# {}代表组，例如{A R D J S}都是效应修饰变量，组里的每个字母用空格隔开
dag <- dagitty:: dagitty("dag{
                         X -> Z -> Y; X <- U -> Y;
                         {A R D J S} -> Y;
                         {B I} -> Z; 
                         I -> X; I -> Y;
                         D -> S; D -> J;
                         R -> Z;
                  }") 
# 使用ggdag包
# 使用tidy_dagitty让原来的dag变成tidy形式，方便使用其他函数来修饰
# 使用dag_label给dag当中的每一个变量搭配唯一一个标签，
# 一定注意中英文输入法状态下的标点符号是不同的，代码需要用英文输入法的符号
# 尤其区分“”和""，两个不同，第一个不对
# seed的值随便，设置seed是为了可重复性，不设置的话，每次的图都有差别
dag_tidy <- ggdag::tidy_dagitty(dag, seed = 4) %>%
  dag_label(labels = c("X" = "Meth Use",  
                       "Y" = "HIV", "U" = "Unobserved Confounder",  
                       "A" = "Age", "B" = "Binge Drink",  
                       "D" = "Education", "J" = "Occupation", 
                       "S" = "Monthly Income", "R" = "Sexual Orientation",  
                       "I" = "Injection Drug", "Z" = "Condomless Sex"))
# 使用ggdag函数来制作DAG
# dag_tidy是你的dag数据，node_size可以修改点的大小，text_size可以修改字母大小
# label_size修改标签大小，edge_type是边的类型，可以是直的，也可以是弯的
# geom_dag_label_repel里面是添加标签的函数，最好不要变动
# geom_dag_edges可以改变边的属性，我这里改变了宽度
# theme_dag_blank可以删掉背景
ggdag(dag_tidy, node_size = 15, text_size = 5, label_size = 5,
      edge_type = "link_arc") +
  geom_dag_label_repel(aes(label = label)) +
  geom_dag_edges(edge_width = 1) +
  theme_dag_blank() +
  expand_plot(expand_x = expansion(c(0.1, 0.1)),
              expand_y = expansion(c(0.1, 0.1)))