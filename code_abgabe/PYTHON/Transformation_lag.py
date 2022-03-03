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
        txt_t = MathTex(r"t_z").next_to(t, direction=UP, buff=0.2)
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
        txt_formula_lead = Tex(r"Lead = lag + 2x Dauer der Ernährung", font_size= 32).move_to(pos_3)

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
        self.play(FadeTransform(VGroup(txt_lag, txt_lead), text_lag_lead))
        self.wait(2)
        Animation_1 = obj_to_move.animate(run_time=7).shift(RIGHT * 2)
        self.play(Animation_1)

        # self.play(FadeOut(VGroup(t,txt_t,l0, txt_t,txt_formula_t,txt_formula_lag,txt_formula_lead, txt1, rectangle_txt,text_lag_lead,rectangle)))
