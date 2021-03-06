# This is a sample Python script.
from manim import *
import numpy as np


class CustomRectangle(Square):
    def custom_method(self, color):
        self.set_color(color)
        self.shift(2 * UP)
        return self

def get_grid(self, sx, ex, dx, sy, ey, dy):
    def get_line(s, e):
            return Line(s, e, color=GREY, stroke_width=1)

    ctp = self.coords_to_point
    v_lines = VGroup(*[get_line(ctp(x, sy), ctp(x, ey)) for x in np.arange(sx, ex + dx, dx)])
    h_lines = VGroup(*[get_line(ctp(sx, y), ctp(ex, y)) for y in np.arange(sy, ey + dy, dy)])

    return VGroup(v_lines, h_lines)


class NumberLineTest(Scene):
    def construct(self):

        # defining the x axis
        l0 = NumberLine(
            x_range=[0, 10, 1],
            length=10,
            color=WHITE,
            include_numbers = False
        )

        # define the x Axis name
        txt1 = Tex(r"Dauer der Beobachtung").next_to(l0, direction=DOWN)

       # define the point/ line t
        t = Line(
            start=[-4, 0, 0],
            end=[-4,2, 0],
            color = WHITE
        )
        txt_t = Tex(r"t").next_to(t, direction=UP, buff=0.2)
        # define the lag
        ln_1 = Line(
            start= [-2,0,0],
            end = [-2,2,0],
            color = RED_B
        )
        # name lag
        txt_lag = MathTex(r"Lag").next_to(ln_1, direction= UP, buff= 0.2).set_color(RED_B)

        ln_3 = Line(
            start=[2, 0, 0],
            end=[2, 2, 0],
            color= RED_B
        )
        txt_lead = MathTex(r"Lead").next_to(ln_3, direction=UP, buff=0.2).set_color(RED_B)

        rectangle_txt = RoundedRectangle (stroke_width= 1, stroke_color= YELLOW, width=8, height=2.8).shift(DOWN *2.5 + LEFT*2.5)

        pos_1 = [-3, -1.5, 0]
        pos_2 = [-4, -2.5, 0]
        pos_3 = [-2.1, -3.5, 0]
        txt_formula_t = Tex(r"t = Tag der Proteinzufuhr", font_size= 32).move_to(pos_1)
        txt_formula_lag = Tex(r"Lag = t + 4", font_size=32).move_to(pos_2)
        txt_formula_lead = Tex(r"Lead = lag + 2x Dauer der Ern??hrung", font_size= 32).move_to(pos_3)

        # the imput graphic is hereby declared
        # now we go to the rectangle window defintion and its movement


        x_mov_rect = []

        rectangle = always_redraw(lambda :
                                  Rectangle(width = 4, height= 2, stroke_color= RED_A, fill_color= RED_C, fill_opacity= 0.5).shift(UP * 1))

        text_lag_lead = always_redraw(lambda :
                                      Tex("Lag-Lead Window", font_size= 48).next_to(rectangle,direction= UP, buff=0.2))

        # help_line = always_redraw(lambda :DashedLine(start=t.get_center(), end=))
        # point_from_proportion()
        obj_to_move = VGroup(rectangle,t,txt_t,text_lag_lead)

        self.wait(3)
        self.play(Write(txt1))
        self.play(Write(l0))
        self.wait(2)
        self.play(Write(VGroup(t, txt_t,rectangle_txt,txt_formula_t)))
        self.wait(2)
        self.play(Write((VGroup(ln_1, txt_lag, txt_formula_lag))))
        self.wait(2)
        self.play(Write(VGroup(ln_3, txt_lead,txt_formula_lead)))
        self.wait(4)
        self.play(FadeOut(VGroup(ln_1, ln_3)))
        self.play(Write(rectangle))
        self.wait(1)
        self.play(FadeTransform(VGroup(txt_lag,txt_lead), text_lag_lead))
        self.wait(2)
        self.play(AnimationGroup(*[
            obj_to_move.animate(run_time = 7).shift(RIGHT *2)
        ]))

        grid = Axes(
            x_range=[0, 28, 4],
            y_range=[0, 11, 1],
            x_length=8,
            y_length=5,
            x_axis_config= {"numbers_to_include": np.arange(4,28,4),
            "font_size": 24,},
            tips=False,).to_edge(RIGHT, buff=0.25)
        box_around = SurroundingRectangle(grid, color= WHITE)

        y_label = grid.get_y_axis_label(r"t_z", edge=LEFT, direction=LEFT, buff=0.2)
        y_label.save_state()
        y_label.generate_target()
        y_label.target.shift(UP * 1.5 + LEFT * 4.2)
        x_label = Tex(r"Dauer der Beatmung $(t_j)$").next_to(grid, direction= DOWN, buff = 0.2)
         # def rectangles_plot_2 ():
        rectangle_1 = Rectangle(width=2, height=0.5, stroke_color=RED_A, fill_color=RED_C,
                                              fill_opacity=0.8).shift(DOWN * 2 + RIGHT).set_fill(BLACK)
        rectangle_2 = Rectangle(width=2.5, height=0.5, stroke_color=RED_A, fill_color=RED_C,
                                              fill_opacity=0.8).shift(DOWN * 1.5 + RIGHT * 2).set_fill(BLACK)
        rectangle_3 = Rectangle(width=3, height=0.5, stroke_color=RED_A, fill_color=RED_C,
                                              fill_opacity=0.8).shift((DOWN * 1) + RIGHT * 3).set_fill(BLACK)
        rectangle_4 = Rectangle(width=3.5, height=0.5, stroke_color=RED_A, fill_color=RED_C,
                                              fill_opacity=0.8).shift((DOWN * 0.5) + RIGHT * 4).set_fill(BLACK)
        rectangle_5 = Rectangle(width=4, height=0.5, stroke_color=RED_A, fill_color=RED_C,
                                              fill_opacity=0.8).shift( (RIGHT * 4.9)).set_fill(BLACK)
        rectangle_6 = Rectangle(width=3, height=0.5, stroke_color=RED_A, fill_color=RED_C,
                                              fill_opacity=0.8).shift(UP * 0.5  + RIGHT * 5.4).set_fill(BLACK)
        rectangle_7 = Rectangle(width=2, height=0.5, stroke_color=RED_A, fill_color=RED_C,
                                              fill_opacity=0.8).shift(UP  + RIGHT * 5.9).set_fill(BLACK)
        rectangle_8 = Rectangle(width=1, height=0.5, stroke_color=RED_A,
                                fill_opacity=2).shift(UP * 1.5 + RIGHT * 6.4).set_fill(BLACK)

        rectangles_all = VGroup(rectangle_1, rectangle_2, rectangle_3, rectangle_4, rectangle_5,rectangle_6, rectangle_7,rectangle_8)

        rectangles_all_plot = VGroup(rectangle_1,rectangle_2, rectangle_3,rectangle_4, rectangle_5,grid, y_label,x_label)

        # header for plot

        header_1 = Tex("Lag-Lead Window f??r einen Patienten").next_to(grid, direction=UP, buff=0.5)
        header_2 = Tex("Partielle Effekte").next_to(grid, direction=UP, buff=0.5)

        # textbox
        rectangle_txt_2 = RoundedRectangle(stroke_width=1, stroke_color=YELLOW, width=5, height=4).shift(LEFT * 4.5)

        pos_text_1 = [-4, 3, 0]
        pos_text_2 = [-4.5, 0, 0]
        pos_text_3 = [-4.9, -1, 0]

        tz = Tex(r"$t_z$: Protokoll Tag",font_size= 32).shift(UP * 1.3 + LEFT * 5)
        ztz = Tex(r"$z(t_z)$: Wert der Proteinzufuhr",font_size= 32).shift(pos_text_2)
        timet = Tex(r"$t$: Beatmungsdauer",font_size= 32).shift(pos_text_3)

        part_formula = Tex(r"$h(t,t_z,z(t_z))$",font_size= 40).shift(pos_text_2).set_color(WHITE)
        part_box = SurroundingRectangle(part_formula, color = GOLD_A)

        text_group = VGroup(rectangle_txt_2,tz,ztz,timet, y_label.target)

        # self.play(FadeOut(VGroup(t,txt_t,l0, txt_t,txt_formula_t,txt_formula_lag,txt_formula_lead, txt1, rectangle_txt,text_lag_lead,rectangle)))
        self.play(FadeOut(
            VGroup(l0, txt_t, txt_formula_t, txt_formula_lag, obj_to_move, txt1, rectangle_txt, txt_formula_lead)))
        small_anim = VGroup(rectangle, text_lag_lead)
        self.play(Write(VGroup(grid, y_label, x_label, header_1, box_around)))
        self.wait(2)
        self.play(FadeIn(small_anim.shift(LEFT * 4.5)))
        self.play(FadeTransform(small_anim.shift(LEFT * 4.5),rectangle_1))
        # self.play(Write(VGroup(grid,y_label,x_label, header_1)))
        self.play(Write(VGroup(rectangle_2, rectangle_3, rectangle_4, rectangle_5, rectangle_6, rectangle_7, rectangle_8)))
        self.wait(2)
        self.play(Write(rectangle_txt_2))
        self.play(MoveToTarget(y_label))
        self.play(Transform(y_label.target, tz))
        self.play(Restore(y_label))
        self.wait(2)
        self.play(Write(ztz))
        self.wait(2)
        self.play(Write(timet))
        self.wait(2)
        self.play(FadeTransformPieces(VGroup(text_group,header_1), VGroup( part_formula,header_2)))
        rectangle_1.set_fill(BLUE_A)
        rectangle_2.set_fill(BLUE_B)
        rectangle_3.set_fill(BLUE_C)
        rectangle_4.set_fill(BLUE_D)
        rectangle_5.set_fill(BLUE_E)
        rectangle_6.set_fill(DARK_BLUE)
        rectangle_7.set_fill(BLUE_A)
        rectangle_8.set_fill(BLUE_C)
        self.play(Write(VGroup(rectangle_1,rectangle_2, rectangle_3, rectangle_4, rectangle_5, rectangle_6, rectangle_7, rectangle_8)))
        #self.play(TransformMatchingShapes(VGroup(rectangle_1,rectangle_2, rectangle_3, rectangle_4, rectangle_5, rectangle_6, rectangle_7, rectangle_8),
         #                    (VGroup(rectangle_1,rectangle_2, rectangle_3, rectangle_4, rectangle_5, rectangle_6, rectangle_7, rectangle_8))))
        box_1 = always_redraw( lambda : Square(side_length=0.5).shift((DOWN * 2 + RIGHT * 0.25)).set_color(GREY_BROWN))
        box_2 = always_redraw( lambda : Square(side_length=0.5).next_to(box_1, direction=UP,buff=0).set_color(GREY_BROWN))
        box_3 = always_redraw( lambda : Square(side_length=0.5).next_to(box_2, direction=UP,buff=0).set_color(GREY_BROWN))
        box_4 = always_redraw( lambda : Square(side_length=0.5).next_to(box_3, direction=UP,buff=0).set_color(GREY_BROWN))
        box_5 = always_redraw( lambda : Square(side_length=0.5).next_to(box_4, direction=UP,buff=0).set_color(GREY_BROWN))
        boxes = VGroup(box_1,box_2,box_3,box_4,box_5)

        def_food = Tex(r"$P_2 = (1,1,1,1,1,0,0,0)$", font_size=40, color=LIGHT_GREY).shift(LEFT * 4 + DOWN * 2)
        part_formula.generate_target()
        part_formula.target.shift(UP * 1.9  + RIGHT * 5.75)
        self.play(MoveToTarget(part_formula, run_time=1.5))
        self.wait(2)
        self.play(FadeIn(VGroup(def_food,boxes)))
        self.wait(2)
        cum_effect = Text(r"Kumulative Effekte",font_size= 42).shift(pos_text_1)
        cum_effct_formula= Tex(r"$g_{P_2}(z_{P_2}, t_j)$").next_to(cum_effect, direction=DOWN, buff=0.8)
        cums = VGroup(cum_effect, cum_effct_formula)
        cum_effect_box = SurroundingRectangle(cums).set_color(GREY_BROWN)
        self.play(Write(VGroup(cum_effect_box, cum_effect, cum_effct_formula), run_time=3))

        plane = NumberPlane(y_range=[0,4.75,0.5], x_range=[0,8, 0.5],background_line_style={
                "stroke_color": WHITE,
                "stroke_width": 4,
                "stroke_opacity": 0.2
            }).shift(RIGHT*3 + UP * 0.1).set_color(WHITE)
        # self.play(Write(plane))

        self.wait(1)
        self.play(AnimationGroup(*[
            boxes.animate(run_time=7).shift(RIGHT * 6.4)
        ]))






        # self.play(FadeTransform(rectangles_all_plot,text_group))













